_HEADER = "# DO NOT EDIT: generated by maven_artifact()"
_FETCH_SOURCES_ENV_VAR = "BAZEL_JVM_FETCH_SOURCES"
_MAVEN_WORKSPACE_NAME = "%{maven_workspace_name}"

_JVM_IMPORT_BZL = """
def _jvm_import_impl(ctx):
    if len(ctx.files.jars) != 1:
        fail("Must specify exactly one jar", attr = "jars")
    jar = ctx.files.jars[0]
    return [
        DefaultInfo(
            files = depset(direct = [jar]),
        ),
        JavaInfo(
            output_jar = jar,
            compile_jar = java_common.stamp_jar(ctx.actions, jar = jar, target_label = ctx.label, java_toolchain = ctx.attr._java_toolchain),
            source_jar = getattr(ctx.file, "srcjar", None),
            deps = [d[JavaInfo] for d in getattr(ctx.attr, "deps", [])],
        ),
    ]

jvm_import = rule(
    _jvm_import_impl,
    attrs = {
        "jars": attr.label_list(allow_files = [".jar"]),
        "deps": attr.label_list(providers = [JavaInfo]),
        "srcjar": attr.label(allow_single_file = ["-sources.jar"]),
        "_java_toolchain": attr.label(default = Label("@bazel_tools//tools/jdk:current_java_toolchain")),
    },
)
"""

def _serialize_rule(ctx, name, **kwargs):
    lines = []

    #visibility = ["//:__subpackages__", "//external:__pkg__"]
    visibility = ["//visibility:public"]
    tags = []
    if "alias" in kwargs:
        #visibility += ["@%s//:__subpackages__" % kwargs["alias"]]
        pass
    if "replacement" in kwargs:
        #visibility = ["//visibility:public"]
        lines += [
            "",
            "alias(",
            '    name = "%s",' % name,
            '    actual = "%s",' % kwargs["replacement"],
            "    visibility = [%s]," % _serialize_list(sorted(visibility)),
            ")",
            "",
        ]
    else:
        coord = _decode_maven_coordinates(kwargs["artifact"])
        tags += ["maven_coordinates=%s" % kwargs["artifact"]]
        if kwargs.get("is_root"):
            #visibility = ["//visibility:public"]
            pass
        else:
            tags += ["no-ide"]

        # handle each type of artifact appropriately
        if coord.packaging == "pom":
            lines.append("java_library(")
            lines.append('    name = "%s",' % name)
            lines.append("    exports = [%s]," % _serialize_list(kwargs.get("deps")))
            lines.append("    visibility = [%s]," % _serialize_list(sorted(visibility)))
            lines.append("    tags = [%s]," % _serialize_list(sorted(tags)))
            lines.append(")")
        elif coord.packaging == "aar":
            lines.append("aar_import(")
            lines.append('    name = "%s",' % name)
            lines.append('    aar = "%s",' % _download_artifact(ctx, coord, kwargs["sha256"]))
            lines.append("    visibility = [%s]," % _serialize_list(sorted(visibility)))
            lines.append("    tags = [%s]," % _serialize_list(sorted(tags)))
            lines.append(")")
        elif coord.packaging == "jar":
            lines.append("jvm_import(")
            lines.append('    name = "%s",' % name)
            lines.append('    jars = ["%s"],' % _download_artifact(ctx, coord, kwargs["sha256"]))
            if "sha256_src" in kwargs and ctx.os.environ.get(_FETCH_SOURCES_ENV_VAR, "true").lower() == "true":
                lines.append('    srcjar = "%s",' % _download_artifact(ctx, coord, kwargs["sha256_src"], classifier = "sources"))
            lines.append("    deps = [%s]," % _serialize_list(kwargs.get("deps")))
            lines.append("    visibility = [%s]," % _serialize_list(sorted(visibility)))
            lines.append("    tags = [%s]," % _serialize_list(sorted(tags)))
            lines.append(")")
        else:
            fail("Unsupported packaging '%s' (expected 'pom', 'jar' or 'aar')" % coord.packaging, attr = "artifact")
        lines.append("")
    return lines

def _imports_repository_impl(ctx):
    ctx.file("BUILD", "")
    ctx.file("jvm_import.bzl", _JVM_IMPORT_BZL)

    # group targets by package
    targets_by_package = {}
    for label, kwargs in _DEPENDENCIES.items():
        label_parts = label.split(":", 1)
        pkg = label_parts[0][len("//"):]
        name = label_parts[1]
        if pkg not in targets_by_package:
            targets_by_package[pkg] = {}
        if name in targets_by_package[pkg]:
            fail("Duplicate targets in package (this is an implementation error)")
        targets_by_package[pkg][name] = kwargs

    # write BUILD file for each package
    for pkg, targets in targets_by_package.items():
        lines = [
            _HEADER,
            "",
            #'package(default_visibility = ["//:__subpackages__"])',
            'package(default_visibility = ["//visibility:public"])',
            "",
            'load("//:jvm_import.bzl", "jvm_import")',
            "",
        ]
        for name in sorted(targets.keys()):
            lines += _serialize_rule(ctx, name, **targets[name])
        ctx.file("%s/BUILD" % pkg, "\n".join(lines))
    return

_imports_repository = repository_rule(
    _imports_repository_impl,
    environ = [_FETCH_SOURCES_ENV_VAR],
)

def _download_artifact(ctx, coord, sha256, classifier = None):
    classifier = classifier or coord.classifier or ""
    if classifier:
        classifier = "-" + classifier
    group_id = coord.group_id.replace(".", "/")
    version = coord.version
    packaging = coord.packaging
    artifact_id = coord.artifact_id
    basename = artifact_id + "-" + version + classifier + "." + packaging
    url_suffix = group_id + "/" + artifact_id + "/" + version + "/" + basename
    urls = []
    for server_url in _REPOSITORIES:
        if not server_url.endswith("/"):
            urls += [server_url + "/" + url_suffix]
        else:
            urls += [server_url + url_suffix]
    ctx.download(urls, output = coord.group_id + "/" + basename, sha256 = sha256)
    return basename

def _decode_maven_coordinates(artifact, default_packaging = "jar"):
    parts = artifact.split(":")
    group_id = parts[0]
    artifact_id = parts[1]
    version = parts[2]
    classifier = None
    packaging = default_packaging
    if len(parts) == 4:
        packaging = parts[2]
        version = parts[3]
    elif len(parts) == 5:
        packaging = parts[2]
        classifier = parts[3]
        version = parts[4]
    return struct(
        group_id = group_id,
        artifact_id = artifact_id,
        version = version,
        classifier = classifier,
        packaging = packaging,
    )

def _serialize_list(items):
    if not items:
        return ""
    return "\n        " + "    ".join([
        '"%s",\n    ' % i
        for i in items or []
    ])

def maven_dependencies():
    # do 'jvm_import' graph repository
    _imports_repository(
        name = _MAVEN_WORKSPACE_NAME,
    )
