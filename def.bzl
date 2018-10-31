load("//3rdparty:workspace.bzl", _maven_dependencies = "maven_dependencies")

bazeldeps_dependencies = _maven_dependencies

def _impl(ctx):
    """
    """
    runfiles = []
    transitive_runfiles = []

    runfiles.append(ctx.file.deps_file)
    transitive_runfiles.append(ctx.attr._parseproject.default_runfiles.files)
    transitive_runfiles.append(ctx.attr._parseproject.data_runfiles.files)

    ctx.actions.expand_template(
        template = ctx.file._launcher_template,
        output = ctx.outputs.executable,
        substitutions = {
            "%{deps_file}": ctx.file.deps_file.short_path,
            "%{sha_file}": ctx.label.package + "/" + ctx.attr.sha_file,
            "%{default_command}": ctx.attr.default_command,
        },
        is_executable = True,
    )

    return [DefaultInfo(
        runfiles = ctx.runfiles(
            files = runfiles,
            transitive_files = depset(transitive = transitive_runfiles),
        ),
    )]

bazeldeps = rule(
    implementation = _impl,
    attrs = {
        "default_command": attr.string(
            default = "generate",
            mandatory = False,
        ),
        "sha_file": attr.string(
            default = "3rdparty/workspace.bzl",
        ),
        "deps_file": attr.label(
            default = "dependencies.yaml",
            allow_single_file = [".yml", ".yaml", ".json"],
        ),
        "_parseproject": attr.label(
            default = Label("//src/scala/com/github/johnynek/bazel_deps:parseproject"),
            executable = True,
            cfg = "host",
        ),
        "_launcher_template": attr.label(
            default = "//:launcher.sh.tpl",
            allow_single_file = True,
        ),
    },
    executable = True,
)
