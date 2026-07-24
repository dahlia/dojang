#!/usr/bin/env python3
"""Integration tests for the verified release installers."""

from __future__ import annotations

import hashlib
import http.server
import os
from pathlib import Path
import shutil
import socketserver
import subprocess
import tarfile
import tempfile
import threading
import unittest
import zipfile


REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
VERSION = "1.2.3"
POSIX_PLATFORMS = (
    ("linux", "x86_64"),
    ("linux", "aarch64"),
    ("macos", "x86_64"),
    ("macos", "aarch64"),
)


class QuietHandler(http.server.SimpleHTTPRequestHandler):
    """Serves fixture releases without writing request logs."""

    def log_message(self, format: str, *args: object) -> None:
        pass


class ThreadingServer(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True


class InstallerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.release_directory = self.root / VERSION
        self.release_directory.mkdir()
        self.expected_contents: dict[str, bytes] = {}
        self._make_release()
        handler = lambda *args, **kwargs: QuietHandler(
            *args, directory=str(self.root), **kwargs
        )
        self.server = ThreadingServer(("127.0.0.1", 0), handler)
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()
        host, port = self.server.server_address
        self.base_url = f"http://{host}:{port}"

    def tearDown(self) -> None:
        self.server.shutdown()
        self.server.server_close()
        self.server_thread.join()
        self.temporary_directory.cleanup()

    def _make_release(self) -> None:
        checksums: list[str] = []
        for operating_system, architecture in POSIX_PLATFORMS:
            asset = (
                f"dojang-{VERSION}-{operating_system}-{architecture}.tar.xz"
            )
            contents = f"{operating_system}-{architecture}\n".encode()
            self.expected_contents[asset] = contents
            source = self.root / "dojang"
            source.write_bytes(contents)
            source.chmod(0o755)
            with tarfile.open(self.release_directory / asset, "w:xz") as archive:
                archive.add(source, arcname="dojang")
            checksums.append(self._checksum_line(asset))

        windows_asset = f"dojang-{VERSION}-windows-x86_64.zip"
        windows_contents = b"windows-x86_64\r\n"
        self.expected_contents[windows_asset] = windows_contents
        with zipfile.ZipFile(
            self.release_directory / windows_asset, "w"
        ) as archive:
            archive.writestr("dojang.exe", windows_contents)
        checksums.append(self._checksum_line(windows_asset))
        (self.release_directory / "SHA256SUMS").write_text(
            "".join(checksums), encoding="utf-8"
        )

    def _checksum_line(self, asset: str) -> str:
        digest = hashlib.sha256(
            (self.release_directory / asset).read_bytes()
        ).hexdigest()
        return f"{digest}  {asset}\n"

    def _environment(self, install_directory: Path) -> dict[str, str]:
        return {
            **os.environ,
            "DOJANG_INSTALL_BASE_URL": self.base_url,
            "DOJANG_INSTALL_VERSION": VERSION,
            "DOJANG_INSTALL_DIR": str(install_directory),
        }

    @unittest.skipUnless(shutil.which("sh"), "POSIX shell is unavailable")
    def test_posix_installer_verifies_every_supported_artifact(self) -> None:
        for operating_system, architecture in POSIX_PLATFORMS:
            with self.subTest(
                operating_system=operating_system,
                architecture=architecture,
            ):
                install_directory = (
                    self.root / "installed" / operating_system / architecture
                )
                environment = self._environment(install_directory)
                environment["DOJANG_INSTALL_OS"] = operating_system
                environment["DOJANG_INSTALL_ARCH"] = architecture
                subprocess.run(
                    ["sh", str(REPOSITORY_ROOT / "scripts/install.sh")],
                    check=True,
                    env=environment,
                    capture_output=True,
                    text=True,
                )
                asset = (
                    f"dojang-{VERSION}-{operating_system}-{architecture}.tar.xz"
                )
                installed = install_directory / "dojang"
                self.assertEqual(installed.read_bytes(), self.expected_contents[asset])
                self.assertTrue(installed.stat().st_mode & 0o111)

    @unittest.skipUnless(shutil.which("sh"), "POSIX shell is unavailable")
    def test_posix_installer_rejects_a_checksum_mismatch(self) -> None:
        asset = f"dojang-{VERSION}-linux-x86_64.tar.xz"
        (self.release_directory / "SHA256SUMS").write_text(
            f"{'0' * 64}  {asset}\n", encoding="utf-8"
        )
        install_directory = self.root / "rejected"
        environment = self._environment(install_directory)
        environment["DOJANG_INSTALL_OS"] = "linux"
        environment["DOJANG_INSTALL_ARCH"] = "x86_64"
        result = subprocess.run(
            ["sh", str(REPOSITORY_ROOT / "scripts/install.sh")],
            check=False,
            env=environment,
            capture_output=True,
            text=True,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((install_directory / "dojang").exists())

    @unittest.skipUnless(shutil.which("sh"), "POSIX shell is unavailable")
    def test_posix_installer_accepts_binary_uppercase_checksums(self) -> None:
        asset = f"dojang-{VERSION}-linux-x86_64.tar.xz"
        digest = hashlib.sha256(
            (self.release_directory / asset).read_bytes()
        ).hexdigest()
        (self.release_directory / "SHA256SUMS").write_text(
            f"{digest.upper()} *{asset}\n", encoding="utf-8"
        )
        install_directory = self.root / "binary checksum install"
        environment = self._environment(install_directory)
        environment["DOJANG_INSTALL_OS"] = "linux"
        environment["DOJANG_INSTALL_ARCH"] = "x86_64"
        subprocess.run(
            ["sh", str(REPOSITORY_ROOT / "scripts/install.sh")],
            check=True,
            env=environment,
            capture_output=True,
            text=True,
        )
        self.assertEqual(
            (install_directory / "dojang").read_bytes(),
            self.expected_contents[asset],
        )

    @unittest.skipUnless(shutil.which("sh"), "POSIX shell is unavailable")
    def test_posix_installer_can_only_download_a_verified_archive(self) -> None:
        asset = f"dojang-{VERSION}-linux-x86_64.tar.xz"
        install_directory = self.root / "must not install"
        download_directory = self.root / "POSIX downloads with spaces"
        environment = self._environment(install_directory)
        environment["DOJANG_INSTALL_OS"] = "linux"
        environment["DOJANG_INSTALL_ARCH"] = "x86_64"
        environment["DOJANG_INSTALL_DOWNLOAD_DIR"] = str(download_directory)
        subprocess.run(
            ["sh", str(REPOSITORY_ROOT / "scripts/install.sh")],
            check=True,
            env=environment,
            capture_output=True,
            text=True,
        )
        self.assertEqual(
            (download_directory / asset).read_bytes(),
            (self.release_directory / asset).read_bytes(),
        )
        self.assertEqual(
            (download_directory / "SHA256SUMS").read_bytes(),
            (self.release_directory / "SHA256SUMS").read_bytes(),
        )
        self.assertFalse((install_directory / "dojang").exists())

    @unittest.skipUnless(shutil.which("sh"), "POSIX shell is unavailable")
    def test_posix_installer_requires_home_for_the_default_directory(self) -> None:
        environment = self._environment(self.root / "unused")
        environment.pop("DOJANG_INSTALL_DIR")
        environment.pop("HOME", None)
        environment["DOJANG_INSTALL_OS"] = "linux"
        environment["DOJANG_INSTALL_ARCH"] = "x86_64"
        result = subprocess.run(
            ["sh", str(REPOSITORY_ROOT / "scripts/install.sh")],
            check=False,
            env=environment,
            capture_output=True,
            text=True,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("HOME", result.stderr)

    @unittest.skipUnless(shutil.which("pwsh"), "PowerShell is unavailable")
    def test_powershell_installer_verifies_the_windows_artifact(self) -> None:
        install_directory = self.root / "Windows install"
        environment = self._environment(install_directory)
        environment["DOJANG_INSTALL_ARCH"] = "x86_64"
        subprocess.run(
            [
                "pwsh",
                "-NoLogo",
                "-NoProfile",
                "-File",
                str(REPOSITORY_ROOT / "scripts/install.ps1"),
            ],
            check=True,
            env=environment,
            capture_output=True,
            text=True,
        )
        asset = f"dojang-{VERSION}-windows-x86_64.zip"
        self.assertEqual(
            (install_directory / "dojang.exe").read_bytes(),
            self.expected_contents[asset],
        )

    @unittest.skipUnless(shutil.which("pwsh"), "PowerShell is unavailable")
    def test_powershell_installer_rejects_a_checksum_mismatch(self) -> None:
        asset = f"dojang-{VERSION}-windows-x86_64.zip"
        (self.release_directory / "SHA256SUMS").write_text(
            f"{'0' * 64}  {asset}\n", encoding="utf-8"
        )
        install_directory = self.root / "rejected Windows install"
        environment = self._environment(install_directory)
        environment["DOJANG_INSTALL_ARCH"] = "x86_64"
        result = subprocess.run(
            [
                "pwsh",
                "-NoLogo",
                "-NoProfile",
                "-File",
                str(REPOSITORY_ROOT / "scripts/install.ps1"),
            ],
            check=False,
            env=environment,
            capture_output=True,
            text=True,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((install_directory / "dojang.exe").exists())

    @unittest.skipUnless(shutil.which("pwsh"), "PowerShell is unavailable")
    def test_powershell_installer_can_only_download_a_verified_archive(
        self,
    ) -> None:
        asset = f"dojang-{VERSION}-windows-x86_64.zip"
        install_directory = self.root / "must not install on Windows"
        download_directory = self.root / "Windows downloads with spaces"
        environment = self._environment(install_directory)
        environment["DOJANG_INSTALL_ARCH"] = "x86_64"
        environment["DOJANG_INSTALL_DOWNLOAD_DIR"] = str(download_directory)
        subprocess.run(
            [
                "pwsh",
                "-NoLogo",
                "-NoProfile",
                "-File",
                str(REPOSITORY_ROOT / "scripts/install.ps1"),
            ],
            check=True,
            env=environment,
            capture_output=True,
            text=True,
        )
        self.assertEqual(
            (download_directory / asset).read_bytes(),
            (self.release_directory / asset).read_bytes(),
        )
        self.assertEqual(
            (download_directory / "SHA256SUMS").read_bytes(),
            (self.release_directory / "SHA256SUMS").read_bytes(),
        )
        self.assertFalse((install_directory / "dojang.exe").exists())


if __name__ == "__main__":
    unittest.main()
