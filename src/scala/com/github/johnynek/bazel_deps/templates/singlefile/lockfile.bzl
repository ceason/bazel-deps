load(":internal.bzl", _maven_artifact = "maven_artifact")

def maven_dependencies(replacements = {}):
    m2_repos = sorted(_REPOSITORIES.values())
    for name, kwargs in _DEPENDENCIES.items():
        if name in replacements:
            _maven_artifact(name = name, replacement = replacements[name])
        else:
            _maven_artifact(name = name, repositories = m2_repos, **kwargs)
    return
