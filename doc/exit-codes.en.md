Exit codes
==========

When the `dojang` command terminates due to an error,
it returns the following exit codes, depending on the situation.

 -  -1: Unexpected error.
 -  1: Invalid command options or arguments.
 -  2: File write failure.
 -  10: The manifest file is missing or the repository is not initialized.
 -  11: The manifest file cannot be read due to problems such as invalid format
    or permissions.
 -  12: Unable to create a new manifest file because it already exists.
 -  20: The env file is missing.
 -  21: The env file cannot be read due to invalid format or permissions.
 -  30: The change cannot be applied because there are conflicting changes
    between the repository and the target.
