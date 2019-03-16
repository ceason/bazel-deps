_HEADER = "# DO NOT EDIT: generated by jvm_import_external()"
_FETCH_SOURCES_ENV_VAR = "BAZEL_JVM_FETCH_SOURCES"

def _jvm_import_impl(ctx):
    if not (ctx.attr.jars or ctx.attr.exports):
        fail("Must specify at least one of 'jars' or 'exports'")
    if len(ctx.files.jars) == 0:
        if len(getattr(ctx.attr, "deps", [])) > 0:
            fail("May not specify 'deps' without also specifying 'jars'", attr = "deps")
        return [
            java_common.merge([
                d[JavaInfo]
                for d in ctx.attr.exports
            ]),
        ]
    elif len(ctx.files.jars) == 1:
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
                exports = [d[JavaInfo] for d in getattr(ctx.attr, "exports", [])],
            ),
        ]
    else:
        fail("Must provide at most one jar", attr = "jars")
    return

java_import = rule(
    _jvm_import_impl,
    attrs = {
        "jars": attr.label_list(allow_files = [".jar"]),
        "deps": attr.label_list(providers = [JavaInfo]),
        "exports": attr.label_list(providers = [JavaInfo]),
        "srcjar": attr.label(allow_single_file = ["-sources.jar"]),
        "_java_toolchain": attr.label(
            default = Label("@bazel_tools//tools/jdk:current_java_toolchain"),
        ),
    },
)

def _jarimport_buildfile_content(
        internal_lib = None,
        name = None,
        jar = None,
        srcjar = None,
        deps = None,
        artifact = None):
    return """
load(":internal.bzl", "java_import")
package(default_visibility = ["//visibility:public"])

java_import(
    name = {name},
    jars = [{jar}],
    srcjar = {srcjar},
    deps = [{deps}],
    tags = [
        "maven_coordinates={artifact}",
    ],
)
""".format(
        name = '"%s"' % name,
        jar = '"%s"' % jar,
        srcjar = '"%s"' % srcjar if srcjar else "None",
        artifact = artifact,
        deps = "\n        " + "\n        ".join([
            '"%s",' % d
            for d in sorted(deps)
        ]) + "\n    ",
    )

def _pompkg_buildfile_content(
        name = None,
        exports = None):
    return """
package(default_visibility = ["//visibility:public"])

java_library(
    name = {name},
    exports = [{exports}],
)
""".format(
        name = '"%s"' % name,
        artifact = artifact,
        exports = "\n        " + "\n        ".join([
            '"%s",' % d
            for d in exports
        ]) + "\n    ",
    )

def _replacement_buildfile_content(
        name = None,
        replacement = None):
    return """
package(default_visibility = ["//visibility:public"])

alias(
    name = {name},
    actual = {actual},
)

""".format(
        name = '"%s"' % name,
        actual = '"%s"' % replacement,
    )

def _download_artifact(
        repository_ctx,
        server_urls = [],
        sha256 = None,
        group_id = None,
        artifact_id = None,
        version = None,
        classifier = None,
        packaging = None):
    group_id = group_id.replace(".", "/")
    if classifier:
        classifier = "-" + classifier
    else:
        classifier = ""
    final_name = artifact_id + "-" + version + classifier + "." + packaging
    url_suffix = group_id + "/" + artifact_id + "/" + version + "/" + final_name
    urls = []
    for server_url in server_urls:
        if not server_url.endswith("/"):
            urls += [server_url + "/" + url_suffix]
        else:
            urls += [server_url + url_suffix]

    repository_ctx.download(urls, output = final_name, sha256 = sha256)
    return final_name

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

def _should_fetch_sources_in_current_env(repository_ctx):
    return repository_ctx.os.environ.get(_FETCH_SOURCES_ENV_VAR, "true").lower() == "true"

def _serialize_rule_import(import_attr, name, path, srcpath, attrs, props, additional_rule_attrs):
    lines = [
        "%s(" % rule_name,
        "    name = %s," % repr(name),
        "    " + import_attr % repr(path) + ",",
    ]
    if srcpath:
        lines.append("    srcjar = %s," % repr(srcpath))
    for prop in props:
        value = getattr(attrs, prop, None)
        if value:
            if prop.endswith("_"):
                prop = prop[:-1]
            lines.append("    %s = %s," % (prop, repr(value)))
    for attr_key in additional_rule_attrs:
        lines.append("    %s = %s," % (attr_key, additional_rule_attrs[attr_key]))
    lines.append(")")
    lines.append("")
    return lines

def _maven_artifact_impl(ctx):
    # check inputs are valid (and disjoint where applicable)
    if (ctx.attr.sha1 or ctx.attr.sha1_src) and (ctx.attr.sha256 or ctx.attr.sha256_src):
        fail("May only specify 'sha1[_src]' or 'sha256[_src]' but not both")

    # repository name
    name = ctx.attr.name
    coord = _decode_maven_coordinates(ctx.attr.artifact)

    # maybe download jars
    srcjar = None
    jar = None
    if ctx.attr.sha256_src and _should_fetch_sources_in_current_env(ctx):
        srcjar = _download_artifact(
            ctx,
            server_urls = ctx.attr.repositories,
            sha256 = ctx.attr.sha256_src,
            group_id = coord.group_id,
            artifact_id = coord.artifact_id,
            version = coord.version,
            packaging = coord.packaging,
            classifier = "sources",
        )
    if ctx.attr.sha256:
        jar = _download_artifact(
            ctx,
            server_urls = ctx.attr.repositories,
            sha256 = ctx.attr.sha256_src,
            group_id = coord.group_id,
            artifact_id = coord.artifact_id,
            version = coord.version,
            packaging = coord.packaging,
            classifier = coord.classifier,
        )

    if ctx.attr.replacement:
        # is it replacement (replacement attr)
        ctx.file("BUILD", _replacement_buildfile_content(
            name = name,
            replacement = ctx.attr.replacement,
        ))
    elif not ctx.attr.sha256:
        # is it something to just 'export' (pom packaging, no jars)
        ctx.file("BUILD", _pompkg_buildfile_content(
            name = name,
            exports = ctx.attr.exports,
        ))
    else:
        # is it import jar (maybe with deps)?
        internal_lib_path = ctx.path(Label(":internal.bzl"))
        ctx.symlink(Label(":internal.bzl"), "internal.bzl")
        ctx.file("BUILD", _jarimport_buildfile_content(
            internal_lib = None,
            name = name,
            jar = jar,
            srcjar = srcjar,
            deps = ctx.attr.deps,
            artifact = ctx.attr.artifact,
        ))

    # the coord's packaging is also an alias, to mimic behavior of native.maven_jar
    ctx.file("%s/BUILD" % coord.packaging, "\n".join([
        "",
        """package(default_visibility = ["//visibility:public"])""",
        "",
        "alias(",
        "    name = \"%s\"," % coord.packaging,
        "    actual = \"@%s\"," % ctx.name,
        ")",
        "",
    ]))

maven_artifact = repository_rule(
    _maven_artifact_impl,
    attrs = {
        "replacement": attr.string(),
        "artifact": attr.string(),
        "deps": attr.string_list(),
        "exports": attr.string_list(),
        "repositories": attr.string_list(doc = "Resolver URLs"),
        "sha256": attr.string(),
        "sha256_src": attr.string(),
    },
    environ = [_FETCH_SOURCES_ENV_VAR],
)
