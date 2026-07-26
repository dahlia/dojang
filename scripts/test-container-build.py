#!/usr/bin/env python3
"""Checks the pinned, multi-architecture container build contract."""

from pathlib import Path
import re
import unittest


REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
DOCKERFILE = REPOSITORY_ROOT / "Dockerfile"
BUILD_WORKFLOW = REPOSITORY_ROOT / ".github" / "workflows" / "build.yaml"
WINDOWS_SMOKE_TEST = REPOSITORY_ROOT / "scripts" / "smoke-test-dist.ps1"
POSIX_SMOKE_TEST = REPOSITORY_ROOT / "scripts" / "smoke-test-dist.sh"
MONAD_FILE_SYSTEM = REPOSITORY_ROOT / "src" / "Dojang" / "MonadFileSystem.hs"
BOOTSTRAP = REPOSITORY_ROOT / "src" / "Dojang" / "Bootstrap.hs"


def workflow_job(contents: str, name: str) -> str:
    """Returns one top-level job from a GitHub Actions workflow."""
    marker = re.search(rf"(?m)^  {re.escape(name)}:\n", contents)
    if marker is None:
        raise AssertionError(f"workflow job not found: {name}")
    start = marker.start()
    following = contents[marker.end() :]
    next_job = re.search(r"(?m)^  [A-Za-z0-9_-]+:\n", following)
    if next_job is None:
        return contents[start:]
    return contents[start : marker.end() + next_job.start()]


class ContainerBuildTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.contents = DOCKERFILE.read_text(encoding="utf-8")
        cls.workflow = BUILD_WORKFLOW.read_text(encoding="utf-8")
        cls.windows_smoke_test = WINDOWS_SMOKE_TEST.read_text(encoding="utf-8")
        cls.posix_smoke_test = POSIX_SMOKE_TEST.read_text(encoding="utf-8")
        cls.monad_file_system = MONAD_FILE_SYSTEM.read_text(encoding="utf-8")
        cls.bootstrap = BOOTSTRAP.read_text(encoding="utf-8")

    def test_workflow_job_ignores_nested_keys(self) -> None:
        workflow = """\
jobs:
  check:
    steps:
      - with:
          build:
  build:
    runs-on: ubuntu-latest
"""
        self.assertEqual(
            workflow_job(workflow, "build"),
            "  build:\n    runs-on: ubuntu-latest\n",
        )

    def test_pins_official_tool_versions_and_checksums(self) -> None:
        expected_values = (
            "GHCUP_VERSION=0.1.40.0",
            "GHCUP_METADATA_COMMIT=4ad95215c0f869d5da04ff05e70b60027e20547f",
            "GHCUP_METADATA_SHA256=a4db470bb1fa67e71df84acd24b023779a9d6c4633d3e6ca90976ee1529e7922",
            "GHC_VERSION=9.10.3",
            "STACK_VERSION=3.11.1",
            "70ca52b73ee796f5c43b4259f7fcedc2a0d60d85a6a9ed40a82ea8553fca34a0",
            "86df64134ab8ca6d4e8b0980e94fc36a447ff09ea823885034a3dd5617e840f3",
            "0253c087da23aabfb6521e741cd3a35d3fcde201d74e34cd293b2bbde722432d",
            "4866d3b241c59860f2f6eaa2a5e96632113e57100b911ff8aa936665cc0045fe",
            "1fda71e657cd8d355625cc66b61b352699279dfee2664c014a392163bd19a952",
            "1617ae9976a5cd38ad4daec583b026b589eb45d5482afb045cd4ca8c8d0de6d0",
        )
        for value in expected_values:
            with self.subTest(value=value):
                self.assertIn(value, self.contents)
        self.assertGreaterEqual(self.contents.count("sha256sum -c"), 4)

    def test_uses_verified_binaries_without_a_remote_shell_pipe(self) -> None:
        self.assertNotRegex(self.contents, re.compile(r"curl[^\n|]*\|\s*(ba)?sh"))
        self.assertIn("ghcup -o -n install ghc -u", self.contents)
        self.assertIn("ghcup -o set ghc", self.contents)
        self.assertNotIn("ghcup-metadata/master", self.contents)
        self.assertIn("--system-ghc", self.contents)
        self.assertIn("--no-install-ghc", self.contents)

    def test_builds_and_tests_with_stack(self) -> None:
        self.assertRegex(self.contents, re.compile(r"\bstack test\b"))
        self.assertNotIn("cabal v2-update", self.contents)
        self.assertNotIn("cabal v2-install", self.contents)

    def test_dependency_layer_supplies_local_package_paths(self) -> None:
        dependency_build = self.contents.index("--only-dependencies")
        self.assertLess(
            self.contents.index("mkdir -p app cbits src test"),
            dependency_build,
        )
        self.assertLess(
            self.contents.index(
                "touch CHANGES.md LICENSE README.md cbits/filesystem.c"
            ),
            dependency_build,
        )
        self.assertIn(
            "RUN set -eux; \\\n    mkdir -p app cbits src test",
            self.contents[:dependency_build],
        )

    def test_runs_tests_as_an_unprivileged_user(self) -> None:
        self.assertIn("adduser -D builder", self.contents)
        self.assertIn("USER builder", self.contents)
        self.assertIn('ENV HOME="/home/builder"', self.contents)
        self.assertIn("--mount=type=tmpfs,target=/tmp", self.contents)
        self.assertIn('TMPDIR="/tmp" stack test', self.contents)
        self.assertIn("ln -s /bin/false /usr/bin/false", self.contents)

    def test_release_workflow_extracts_and_checks_entrypoint_image(self) -> None:
        self.assertIn("docker run --rm --entrypoint cat", self.workflow)
        self.assertIn(
            'docker run --rm "$image" version', self.workflow
        )
        self.assertIn("test -s", self.workflow)

    def test_release_workflow_smoke_tests_every_final_binary_archive(
        self,
    ) -> None:
        portable_build = workflow_job(self.workflow, "build")
        linux_build = workflow_job(self.workflow, "build-linux")
        self.assertIn("shopt -s nullglob", portable_build)
        self.assertIn("scripts/smoke-test-dist.sh", portable_build)
        self.assertIn(r"scripts\smoke-test-dist.ps1", portable_build)
        self.assertIn("shopt -s nullglob", linux_build)
        self.assertIn("scripts/smoke-test-dist.sh", linux_build)

    def test_static_link_check_requires_readelf_to_succeed(self) -> None:
        self.assertNotIn("! readelf", self.contents)
        self.assertIn(
            "readelf -l /out/dojang > /tmp/dojang-headers.txt",
            self.contents,
        )
        self.assertIn(
            "grep -q \"Program Headers\" /tmp/dojang-headers.txt",
            self.contents,
        )
        self.assertIn(
            "! grep -q INTERP /tmp/dojang-headers.txt",
            self.contents,
        )

    def test_archive_smoke_tests_bootstrap_a_repository(self) -> None:
        for script in (self.posix_smoke_test, self.windows_smoke_test):
            with self.subTest(script=script[:20]):
                self.assertIn("--from", script)
                self.assertIn("--yes", script)

    def test_windows_smoke_test_isolates_profile_paths(self) -> None:
        self.assertIn("$env:USERPROFILE = $bootstrapHome", self.windows_smoke_test)
        self.assertIn("$env:APPDATA = $bootstrapHome", self.windows_smoke_test)
        self.assertIn("$env:USERPROFILE = $previousUserProfile", self.windows_smoke_test)
        self.assertIn("$env:APPDATA = $previousAppData", self.windows_smoke_test)

    def test_posix_smoke_test_isolates_config_paths(self) -> None:
        self.assertIn(
            'XDG_CONFIG_HOME="$bootstrap_home/.config"',
            self.posix_smoke_test,
        )

    def test_posix_smoke_test_uses_a_physical_temporary_path(self) -> None:
        allocation = self.posix_smoke_test.index(
            'temporary_directory="$(mktemp -d'
        )
        physical_path = self.posix_smoke_test.index(
            'temporary_directory="$(cd "$temporary_directory" && pwd -P)"'
        )
        self.assertLess(allocation, physical_path)
        self.assertLess(
            physical_path,
            self.posix_smoke_test.index("trap '"),
        )
        self.assertLess(
            physical_path,
            self.posix_smoke_test.index('extracted="$temporary_directory'),
        )

    def test_windows_ffi_selects_the_native_calling_convention(self) -> None:
        self.assertNotIn("foreign import stdcall", self.monad_file_system)
        functions = (
            "ConvertStringSecurityDescriptorToSecurityDescriptorW",
            "LocalFree",
            "GetVolumePathNameW",
            "GetVolumeInformationW",
            "GetFileInformationByHandleEx",
        )
        for function in functions:
            with self.subTest(function=function):
                self.assertIn(
                    f'foreign import ccall unsafe "{function}"',
                    self.monad_file_system,
                )

    def test_zip_validation_uses_cross_platform_entry_metadata(self) -> None:
        self.assertNotIn("Zip.isEntrySymbolicLink", self.bootstrap)
        self.assertIn("unixFileType == 0o120000", self.bootstrap)


if __name__ == "__main__":
    unittest.main()
