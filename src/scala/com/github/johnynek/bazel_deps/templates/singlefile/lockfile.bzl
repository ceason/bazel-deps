load(":internal.bzl", _maven_dependencies = "maven_dependencies")

def maven_dependencies(replacements = {}, aliases = {}):
    _maven_dependencies(
        replacements = replacements,
        aliases = aliases,
        dependencies = _DEPENDENCIES,
        repositories = _REPOSITORIES,
    )
