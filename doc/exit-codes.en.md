Exit codes
==========

When the `dojang` command terminates due to an error,
it returns the following exit codes, depending on the situation.

 -  -1: Unexpected error.
 -  1: Invalid command options or arguments.
 -  2: File write failure.
 -  3: File does not exist.
 -  4: External program terminated with an error (non-zero exit code).
 -  9: Not supported in the environment (platform).
 -  10: The manifest file is missing or the repository is not initialized.
 -  11: The manifest file cannot be read or updated due to problems such as an
    invalid format, permissions, or another filesystem failure.
 -  12: Unable to create a new manifest file because it already exists.
 -  13: Machine state cannot be read or updated because it is missing an
    identity, is malformed, is unsupported, is ambiguous, or is owned by
    another machine.
 -  20: The env file is missing.
 -  21: The env file cannot be read due to invalid format or permissions.
 -  22: Machine facts required for noninteractive enrollment are missing.
 -  30: The change cannot be applied because there are conflicting changes
    between the repository and the target.
 -  31: The file in the repository cannot be the target file.
 -  32: The file is not routed, so the operation cannot be performed.
 -  33: The file is ignored, so the operation cannot be performed.
 -  34: The operation is canceled because target data may be deleted or local
    evidence of a modified destination may be discarded.
 -  35: The route is ambiguous because multiple routes match the same
    destination.
 -  36: The operation is canceled by the user.
 -  37: A lifecycle selection is empty, unknown, or still active.
 -  38: The route configuration cannot give every destination exactly one
    owning route, or a route's source and destination alias each other.
 -  39: A route codec is unknown, invalid, unavailable in the current dry run,
    or failed during forward or reverse evaluation.
 -  40: A hook failed with a non-zero exit code.
