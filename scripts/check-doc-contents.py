#!/usr/bin/env python3
import builtins
from pathlib import Path
from re import compile
from sys import argv, exit, stderr
from typing import AbstractSet, Iterable


class MissingHrefError(ValueError):
    doc_file: Path
    contents_file: Path

    def __init__(self, doc_file: Path, contents_file: Path):
        super().__init__(f"{doc_file} is not linked from {contents_file}")
        self.doc_file = doc_file
        self.contents_file = contents_file


if not hasattr(builtins, "ExceptionGroup"):
    class ExceptionGroup(Exception):
        """A shim for Python 3.11's ExceptionGroup."""

        def __init__(self, message: str, excs: Iterable[Exception]):
            super().__init__(message)
            self.exceptions = tuple(excs)


def check(dir: Path):
    if not dir.is_dir():
        raise ValueError(f"{dir} is not a directory")
    languages = extract_languages(dir.glob("README.*.md"))
    errors = []
    for lang in languages:
        readme_file = dir / f"README.{lang}.md"
        readme = readme_file.read_text(encoding="utf-8")
        hrefs = get_hrefs(readme)
        docs = dir.glob(f"*.{lang}.md")
        for doc in docs:
            if doc == readme_file:
                continue
            if doc.name not in hrefs:
                errors.append(MissingHrefError(doc, readme_file))
        subdir_readmes = dir.glob(f"*/README.{lang}.md")
        for subdir_readme in subdir_readmes:
            if str(subdir_readme.relative_to(readme_file.parent)) not in hrefs:
                errors.append(MissingHrefError(subdir_readme, readme_file))
            try:
                check(subdir_readme.parent)
            except ExceptionGroup as exs:
                errors.extend(exs.exceptions)
            except Exception as ex:
                errors.append(ex)
    if errors:
        raise ExceptionGroup(
            f"there are documents that are not linked by the table of contents",
            errors)


language_suffix_re = compile(r'\.([a-z]{2})\.md$')


def extract_languages(files: Iterable[Path]) -> Iterable[str]:
    re = language_suffix_re
    for f in files:
        m = re.search(f.name)
        if m is None:
            continue
        yield m.group(1)


href_re = compile(r'\[[^[\]]+]\(([^)]+)\)')


def get_hrefs(md: str) -> AbstractSet[str]:
    hrefs = set()
    for m in href_re.finditer(md):
        hrefs.add(m.group(1))
    return hrefs


def main():
    if len(argv) < 2:
        print("Usage:", argv[0], "DIR")
        exit(1)
    try:
        check(Path(argv[1]))
    except ExceptionGroup as exs:
        for ex in exs.exceptions:
            print("Error:", ex, file=stderr)
        exit(1)


if __name__ == "__main__":
    main()
