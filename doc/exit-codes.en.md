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
 -  11: The manifest file cannot be read due to problems such as invalid format
    or permissions.
 -  12: Unable to create a new manifest file because it already exists.
 -  20: The env file is missing.
 -  21: The env file cannot be read due to invalid format or permissions.
 -  30: The change cannot be applied because there are conflicting changes
    between the repository and the target.
 -  31: The file in the repository cannot be the target file.
 -  32: The file is not routed, so the operation cannot be performed.
 -  33: The file is ignored, so the operation cannot be performed.
 -  34: The operation is canceled because some of the target files may be
    deleted.
 -  35: The route is ambiguous because multiple routes match the same
    destination.
 -  36: The operation is canceled by the user.
 -  40: A hook failed with a non-zero exit code.
