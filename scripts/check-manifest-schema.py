#!/usr/bin/env python3
import json
from collections.abc import Iterator
from pathlib import Path
from typing import Any

import regex
from jsonschema import Draft202012Validator, FormatChecker, ValidationError
from jsonschema.validators import extend


class SchemaContractError(AssertionError):
    """Raised when the manifest schema no longer satisfies its contract."""


def iter_references(value: Any) -> Iterator[str]:
    if isinstance(value, dict):
        reference = value.get("$ref")
        if isinstance(reference, str):
            yield reference
        for child in value.values():
            yield from iter_references(child)
    elif isinstance(value, list):
        for child in value:
            yield from iter_references(child)


def resolve_local_reference(schema: dict[str, Any], reference: str) -> None:
    if not reference.startswith("#/"):
        return
    value: Any = schema
    for part in reference[2:].split("/"):
        key = part.replace("~1", "/").replace("~0", "~")
        if not isinstance(value, dict) or key not in value:
            raise SchemaContractError(reference)
        value = value[key]


format_checker = FormatChecker()


@format_checker.checks("regex", raises=regex.error)
def is_regular_expression(value: object) -> bool:
    return not isinstance(value, str) or bool(regex.compile(value))


def validate_pattern(validator, pattern, instance, schema):
    if (
        validator.is_type(instance, "string")
        and regex.search(pattern, instance) is None
    ):
        yield ValidationError(f"{instance!r} does not match {pattern!r}")


schema_file = Path(__file__).parent.parent / "schema" / "manifest.schema.json"
with schema_file.open(encoding="utf-8") as file:
    manifest_schema = json.load(file)

Draft202012Validator.check_schema(
    manifest_schema,
    format_checker=format_checker,
)
for schema_reference in iter_references(manifest_schema):
    resolve_local_reference(manifest_schema, schema_reference)

ManifestValidator = extend(Draft202012Validator, {"pattern": validate_pattern})
validator = ManifestValidator(manifest_schema, format_checker=format_checker)

valid_manifests = [
    {"repository-id": "123e4567-e89b-42d3-a456-426614174000"},
    {"files": {"vimrc": {"linux": "~/.vimrc", "windows": ""}}},
    {
        "files": {
            "vimrc": [
                {"moniker": "linux", "path": ""},
                {"when": "os = windows"},
            ]
        }
    },
    {
        "vars": {
            "CONFIG_HOME": "${XDG_CONFIG_HOME:-$HOME/.config}",
            "PROFILE_HOME": [
                {"moniker": "work", "value": "$HOME/.config-work"},
                {"when": "always", "value": "$CONFIG_HOME"},
            ],
        }
    },
    {
        "files": {
            "id_ed25519": [
                {
                    "moniker": "posix",
                    "path": "~/.ssh/id_ed25519",
                    "mode": "private",
                }
            ],
            "bin/tool": [
                {"when": "always", "path": "~/bin/tool", "mode": "executable"}
            ],
            "vimrc": [
                {"when": "always", "path": "~/.vimrc", "kind": "symlink"},
                {
                    "moniker": "linux",
                    "path": "~/.vimrc",
                    "kind": "symlink",
                    "mode": "default",
                },
                {
                    "when": "always",
                    "path": "~/.vimrc",
                    "codec": "identity",
                },
                {
                    "when": "always",
                    "path": "~/.vimrc",
                    "codec": "template",
                },
                {
                    "when": "always",
                    "path": "~/.vimrc",
                    "codec": {
                        "name": "example",
                        "config": {"profile": "work", "strict": True},
                    },
                },
            ],
        },
        "dirs": {
            "ssh": [
                {"when": "always", "path": "~/.ssh", "mode": "private"},
                {"moniker": "work", "path": "~/.ssh", "mode": "read-only"},
                {"moniker": "posix", "path": "~/.ssh", "kind": "symlink"},
            ]
        },
    },
]
invalid_manifests = [
    {"repository-id": "not-a-uuid"},
    {"files": {"vimrc": "~/.vimrc"}},
    {"files": {"vimrc": [{"path": "~/.vimrc"}]}},
    {
        "files": {
            "vimrc": [
                {"moniker": "linux", "when": "os = linux", "path": "~/.vimrc"}
            ]
        }
    },
    {"files": {"vimrc": [{"when": "always", "path": 1}]}},
    {"files": {"vimrc": [{"when": "always", "unknown": True}]}},
    {"files": {"vimrc": {"123invalid": "~/.vimrc"}}},
    {"vars": {"2HOME": "$HOME"}},
    {"vars": {"HOME": []}},
    {"vars": {"HOME": [{"value": "$HOME"}]}},
    {"vars": {"HOME": [{"when": "always"}]}},
    {
        "vars": {
            "HOME": [
                {"moniker": "linux", "when": "always", "value": "$HOME"}
            ]
        }
    },
    {"vars": {"HOME": [{"when": "always", "value": "$HOME", "x": 1}]}},
    {"unknown": {}},
    {"files": {"vimrc": [{"when": "always", "path": "~/.vimrc", "mode": "0600"}]}},
    {"files": {"vimrc": [{"when": "always", "path": "~/.vimrc", "kind": "hardlink"}]}},
    {"files": {"vimrc": [{"when": "always", "mode": "private"}]}},
    {"files": {"vimrc": [{"when": "always", "kind": "symlink"}]}},
    {"files": {"vimrc": [{"when": "always", "codec": "identity"}]}},
    {
        "files": {
            "vimrc": [
                {
                    "when": "always",
                    "path": "~/.vimrc",
                    "kind": "symlink",
                    "codec": "example",
                }
            ]
        }
    },
    {
        "files": {
            "vimrc": [
                {
                    "when": "always",
                    "path": "~/.vimrc",
                    "kind": "symlink",
                    "codec": {"name": "identity", "config": {"unexpected": True}},
                }
            ]
        }
    },
    {
        "files": {
            "vimrc": [
                {
                    "when": "always",
                    "path": "x",
                    "codec": {
                        "name": "template",
                        "config": {"unexpected": True},
                    },
                }
            ]
        }
    },
    {"files": {"vimrc": [{"when": "always", "path": "x", "codec": ""}]}},
    {
        "files": {
            "vimrc": [
                {
                    "when": "always",
                    "path": "x",
                    "codec": {"name": "example", "extra": True},
                }
            ]
        }
    },
    {
        "files": {
            "vimrc": [
                {
                    "when": "always",
                    "path": "x",
                    "codec": {"name": "example", "config": {"ratio": 1.5}},
                }
            ]
        }
    },
    {
        "files": {
            "vimrc": [
                {
                    "when": "always",
                    "path": "~/.vimrc",
                    "kind": "symlink",
                    "mode": "private",
                    "codec": "example",
                }
            ]
        }
    },
    {"dirs": {"ssh": [{"when": "always", "path": "~/.ssh", "mode": "executable"}]}},
    {
        "dirs": {
            "ssh": [
                {
                    "when": "always",
                    "path": "~/.ssh",
                    "mode": "private-executable",
                }
            ]
        }
    },
]

for valid_manifest in valid_manifests:
    errors = list(validator.iter_errors(valid_manifest))
    if errors:
        raise SchemaContractError(errors)

for invalid_manifest in invalid_manifests:
    if validator.is_valid(invalid_manifest):
        raise SchemaContractError(invalid_manifest)
