_HEADER = "# DO NOT EDIT: generated by maven_artifact()"
_FETCH_SOURCES_ENV_VAR = "BAZEL_JVM_FETCH_SOURCES"

def _java_import_impl(ctx):
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

java_import = rule(
    _java_import_impl,
    attrs = {
        "jars": attr.label_list(allow_files = [".jar"]),
        "deps": attr.label_list(providers = [JavaInfo]),
        "srcjar": attr.label(allow_single_file = ["-sources.jar"]),
        "_java_toolchain": attr.label(default = Label("@bazel_tools//tools/jdk:current_java_toolchain")),
    },
)

def _download_artifact(
        ctx,
        sha256,
        classifier = None):
    coord = _decode_maven_coordinates(ctx.attr.artifact)
    classifier = classifier or coord.classifier or ""
    if classifier:
        classifier = "-" + classifier
    group_id = coord.group_id.replace(".", "/")
    version = coord.version
    packaging = coord.packaging
    artifact_id = coord.artifact_id
    final_name = artifact_id + "-" + version + classifier + "." + packaging
    url_suffix = group_id + "/" + artifact_id + "/" + version + "/" + final_name
    urls = []
    for server_url in ctx.attr.repositories:
        if not server_url.endswith("/"):
            urls += [server_url + "/" + url_suffix]
        else:
            urls += [server_url + url_suffix]
    ctx.download(urls, output = final_name, sha256 = sha256)
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

def _maven_artifact_impl(ctx):
    # TODO: check inputs are valid (and disjoint where applicable)

    # repository name
    name = ctx.attr.name

    buildfile_lines = [
        _HEADER,
        'package(default_visibility = ["//visibility:public"])',
    ]
    if ctx.attr.replacement:
        # is it replacement (replacement attr)
        buildfile_lines += ["""
alias(
    name = "{name}",
    actual = "{actual}",
)""".format(
            name = name,
            actual = ctx.attr.replacement,
        )]
    else:
        coord = _decode_maven_coordinates(ctx.attr.artifact)
        if coord.packaging == "pom":
            # is it something to just 'export' (pom packaging, no jars)
            buildfile_lines += ["""
java_library(
    name = "{name}",
    exports = [{exports}],
)""".format(
                name = name,
                exports = "\n        " + "\n        ".join([
                    '"%s",' % d
                    for d in sorted(ctx.attr.deps)
                ]),
            )]
        elif coord.packaging == "jar":
            srcjar = None
            if ctx.attr.sha256_src and ctx.os.environ.get(_FETCH_SOURCES_ENV_VAR, "true").lower() == "true":
                srcjar = _download_artifact(ctx, ctx.attr.sha256_src, classifier = "sources")
            jar = _download_artifact(ctx, ctx.attr.sha256)
            ctx.symlink(ctx.attr._internal_lib, "internal.bzl")
            buildfile_lines += ["""
load(":internal.bzl", "java_import")

java_import(
    name = "{name}",
    jars = ["{jar}"],
    srcjar = {srcjar},
    tags = ["maven_coordinates={artifact}"],
    deps = [{deps}],
)""".format(
                name = name,
                jar = jar,
                srcjar = '"%s"' % srcjar if srcjar else "None",
                artifact = ctx.attr.artifact,
                deps = "\n        " + "\n        ".join([
                    '"%s",' % d
                    for d in sorted(ctx.attr.deps)
                ]),
            )]
        else:
            fail("Unsupported packaging '%s' (expected 'pom' or 'jar')" % coord.packaging, attr = "artifact")

    ctx.file("BUILD", "\n".join(buildfile_lines))

    # the coord's packaging is also an alias, to mimic behavior of native.maven_jar
    ctx.file("%s/BUILD" % "jar", "\n".join([
        "",
        """package(default_visibility = ["//visibility:public"])""",
        "",
        "alias(",
        "    name = \"%s\"," % "jar",
        "    actual = \"@%s\"," % name,
        ")",
        "",
    ]))
    return

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
        "_internal_lib": attr.label(
            default = "//%{output_path}:internal.bzl",
            allow_single_file = [".bzl"],
        ),
    },
    environ = [_FETCH_SOURCES_ENV_VAR],
)

def maven_dependencies(
        # 'mvn_coord' => 'label' mapping
        replacements = {},
        # '@repository' (or '//external:targetname') => 'mvn_coord' mapping
        aliases = {},
        repositories = {},
        dependencies = {}):
    m2_repos = sorted(repositories.values())

    # print warnings
    missing_alias_coords = {c: None for c in aliases.values() if c not in dependencies}.keys()
    missing_replacement_coords = [c for c in replacements.keys() if c not in dependencies]
    if missing_alias_coords:
        print("\nWARN the following aliases were ignored because they're not listed in dependencies:\n  %s" %
              "\n  ".join(sorted(missing_alias_coords)))
    if missing_replacement_coords:
        print("\nWARN the following replacements were ignored because they're not listed in dependencies:\n  %s" %
              "\n  ".join(sorted(missing_replacement_coords)))

    # do aliases, checking if the alias is a <repoName> or a <bind>
    for name, unversioned_coord in aliases.items():
        if unversioned_coord not in dependencies:
            continue
        actual = "@%s" % dependencies[unversioned_coord]["name"]
        if name.startswith("@"):
            name = name[len("@"):]
            maven_artifact(name = name, replacement = actual)
        elif name.startswith("//external:"):
            name = name[len("//external:"):]
            native.bind(name = name, actual = actual)
        else:
            fail("Bad alias name '%s'. Wanted a bare repository name (eg '@repo_name') or bind target (eg '//external:some/target_name')", attr = "aliases")

    # do dependencies, swapping in replacements as appropriate
    for k, kwargs in dependencies.items():
        if k in replacements:
            maven_artifact(name = kwargs["name"], replacement = replacements[k])
        else:
            maven_artifact(repositories = m2_repos, **kwargs)
    return
