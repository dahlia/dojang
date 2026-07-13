#!/usr/bin/env python3
import json
from pathlib import Path


schema_file = Path(__file__).parent.parent / "schema" / "manifest.schema.json"
with schema_file.open(encoding="utf-8") as file:
    schema = json.load(file)

detailed_path = schema["$defs"]["detailedRouteBranch"]["properties"]["path"]
if detailed_path.get("minLength") != 1:
    raise AssertionError("a detailed route path must not be empty")
