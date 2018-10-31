#!/usr/bin/env bash
[ "$DEBUG" = "1" ] && set -x
set -euo pipefail
err_report() { echo "errexit on line $(caller)" >&2; }
trap err_report ERR

parseproject="$0.runfiles/com_github_johnynek_bazel_deps/src/scala/com/github/johnynek/bazel_deps/parseproject"
deps_file="%{deps_file}" # relative to proj root
sha_file="%{sha_file}"   # relative to proj root
repo_root=$BUILD_WORKSPACE_DIRECTORY
command="%{default_command}"

# override default command if args are specified
if [ $# -gt 0 ]; then
  command=$1; shift
fi

_generate(){
  # todo(ceason): run buildozer 'fix unusedLoads' after generating dir
  # - consider best way to bundle/distrib buildozer with these rules?
  exec "$parseproject" "$command" \
   --repo-root "$repo_root" \
   --deps "$deps_file" \
   --sha-file "$sha_file" \
   "$@" <&0
}

case "$command" in
generate)
  _generate "$@"
  ;;

format-deps)
  exec "$parseproject" "$command" \
   --deps "$repo_root/$deps_file" \
   "$@" <&0
  ;;

add-dep)
  "$parseproject" "$command" \
   --deps "$repo_root/$deps_file" \
   "$@"
  # re-generate deps after adding
  _generate
  ;;

*)
  exec "$parseproject" "$command" "$@" <&0
  ;;

esac




# parse le args
for arg in "$@"; do case $arg in
#	--flagname=*)
#		FLAG_VAR="${arg#*=}"
#		;;
	-h|h|help|--help)
		echo "$USAGE"
		exit 0
		;;
	*)
		>&2 echo "Unknown option '$arg'"
		exit 1
		;;
esac done


