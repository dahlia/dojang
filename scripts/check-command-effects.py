#!/usr/bin/env python3
"""Reject host effects outside the command effect interpreter."""

from pathlib import Path
from re import Pattern, compile
from sys import exit, stderr


ROOT = Path(__file__).resolve().parent.parent
COMMAND_FILES = [ROOT / "src/Dojang/App.hs", ROOT / "src/Dojang/Commands.hs"]
COMMAND_FILES.extend(sorted((ROOT / "src/Dojang/Commands").glob("*.hs")))

FORBIDDEN: tuple[tuple[str, Pattern[str]], ...] = (
    ("direct IO lifting", compile(r"\bliftIO\b")),
    ("direct process exit", compile(r"\bexitWith\b")),
    ("direct process API", compile(r"\bSystem\.Process\b")),
    ("direct environment API", compile(r"\bSystem\.Environment\b")),
    ("direct clock API", compile(r"\bData\.Time\b")),
    ("direct platform API", compile(r"\bSystem\.Info\b")),
    ("direct prompt API", compile(r"\bFortyTwo\b")),
    ("direct text output API", compile(r"\bData\.Text\.IO\b")),
    ("direct terminal API", compile(r"\bhIsTerminalDevice\b")),
)


def is_app_interpreter_boundary(path: Path, line: str) -> bool:
    """Allow App's typed runner adapters and private interpreter state."""
    return path.name == "App.hs" and (
        line.strip() == "import Control.Monad.IO.Class (MonadIO (liftIO))"
        or "either (liftIO . throwIO)" in line
        or line.strip() == "processRef <- liftApp $ liftIO $ newIORef Nothing"
        or line.strip()
        == "let register process = liftApp $ liftIO $ writeIORef processRef $ Just process"
        or line.strip() == "process <- liftApp $ liftIO $ readIORef processRef"
    )


def main() -> None:
    errors: list[str] = []
    for path in COMMAND_FILES:
        for line_number, line in enumerate(
            path.read_text(encoding="utf-8").splitlines(), start=1
        ):
            for description, pattern in FORBIDDEN:
                if pattern.search(line) and not is_app_interpreter_boundary(path, line):
                    relative = path.relative_to(ROOT)
                    errors.append(f"{relative}:{line_number}: {description}: {line.strip()}")

    if errors:
        for error in errors:
            print(error, file=stderr)
        exit(1)


if __name__ == "__main__":
    main()
