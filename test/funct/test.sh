#!/usr/bin/env bash

set -u

app=$(readlink --canonicalize-existing --verbose "$1")

tmpdir=$(mktemp --directory)
trap 'rm -r "${tmpdir}"' EXIT

export OUTDATED_HOME="${tmpdir}"
export OUTDATED_INDEX='https://hackage.haskell.org/packages/index.tar.gz'


main() {
	pushd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null

	test-enoent
	test-einval
	test-enoparse
	test-ok
	test-not-in-index
	test-outdated
	test-multiple-files
	test-os-conf
	test-arch-conf
	test-impl-conf
	test-flags-conf
}

test-enoent() {
	declare -r cabalfile="asset/enoent.cabal"
	declare -r logfile="${tmpdir}/enoent.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the argument is a non-existing file"
	fi
	if ! grep --silent 'openFile: does not exist' "${logfile}"; then
		echo "‘outdated’ is expected to complain when the argument is a non-existing file"
		cat "${logfile}"
	fi
}

test-einval() {
	declare -r cabalfile="asset/einval.cabal"
	declare -r logfile="${tmpdir}/einval.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the argument is garbage"
	fi
	if ! grep --silent 'hGetContents: invalid argument' "${logfile}"; then
		echo "‘outdated’ is expected to complain when asked to read garbage"
		cat "${logfile}"
	fi
}

test-enoparse() {
	declare -r cabalfile="asset/enoparse.cabal"
	declare -r logfile="${tmpdir}/enoparse.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the argument is a non-parsable file"
	fi
	if ! grep --silent "Couldn't parse.*${cabalfile}" "${logfile}"; then
		echo "‘outdated’ is expected to complain when it can't parse the file"
		cat "${logfile}"
	fi
}

test-ok() {
	declare -r cabalfile="asset/ok.cabal"
	declare -r logfile="${tmpdir}/ok.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 0 ]; then
		echo "‘outdated’ is expected to return 0 when the argument is a nice file"
	fi
	if ! grep --silent "^OK" "${logfile}"; then
		echo "‘outdated’ is not expected to complain when the argument is a nice file"
		cat "${logfile}"
	fi
}

test-not-in-index() {
	declare -r cabalfile="asset/not-in-index.cabal"
	declare -r logfile="${tmpdir}/not-in-index.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the argument has an unknown dependency"
	fi
	if ! grep --silent "unknown-dependency.*isn't in the index" "${logfile}"; then
		echo "‘outdated’ is expected to complain when the argument has an unknown dependency"
		cat "${logfile}"
	fi
}

test-outdated() {
	declare -r cabalfile="asset/outdated.cabal"
	declare -r logfile="${tmpdir}/outdated.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the argument has an outdated dependency"
	fi
	if ! grep --silent "text.*does not include the latest version" "${logfile}"; then
		echo "‘outdated’ is expected to complain when the argument has an outdated dependency"
		cat "${logfile}"
	fi
}

test-multiple-files() {
	declare -r cabalfiles=(asset/not-in-index.cabal asset/outdated.cabal)
	declare -r logfile="${tmpdir}/multiple.log"

	$app "${cabalfiles[@]}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the arguments have dependency problems"
	fi
	if ! (grep --silent "text.*does not include the latest version" "${logfile}" && grep --silent "unknown-dependency.*isn't in the index" "${logfile}"); then
		echo "‘outdated’ is expected to complain about all dependency problems"
		cat "${logfile}"
	fi
}

test-os-conf() {
	declare -r cabalfile="asset/os.cabal"
	declare -r logfile="${tmpdir}/os.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 0 ]; then
		echo "‘outdated’ is expected to return 0 with the default configuration"
		cat "${logfile}"
	fi

	$app --conf 'foo' "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the os is configured"
	fi
	if ! grep --silent "text.*does not include the latest version" "${logfile}"; then
		echo "‘outdated’ is expected to complain when the argument has an outdated dependency"
		cat "${logfile}"
	fi
}

test-arch-conf() {
	declare -r cabalfile="asset/arch.cabal"
	declare -r logfile="${tmpdir}/arch.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 0 ]; then
		echo "‘outdated’ is expected to return 0 with the default configuration"
		cat "${logfile}"
	fi

	$app --conf ';bar' "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the arch is configured"
	fi
	if ! grep --silent "text.*does not include the latest version" "${logfile}"; then
		echo "‘outdated’ is expected to complain when the argument has an outdated dependency"
		cat "${logfile}"
	fi
}

test-impl-conf() {
	declare -r cabalfile="asset/impl.cabal"
	declare -r logfile="${tmpdir}/impl.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 0 ]; then
		echo "‘outdated’ is expected to return 0 with the default configuration"
		cat "${logfile}"
	fi

	$app --conf ';;baz-3000' "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the implementation is configured"
	fi
	if ! grep --silent "text.*does not include the latest version" "${logfile}"; then
		echo "‘outdated’ is expected to complain when the argument has an outdated dependency"
		cat "${logfile}"
	fi
}

test-flags-conf() {
	declare -r cabalfile="asset/flags.cabal"
	declare -r logfile="${tmpdir}/flags.log"

	$app "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 0 ]; then
		echo "‘outdated’ is expected to return 0 with the default configuration"
		cat "${logfile}"
	fi

	$app --conf ';;;+qux,-quux' "${cabalfile}" > "${logfile}"

	if [ "$?" -ne 1 ]; then
		echo "‘outdated’ is expected to return 1 when the flags are configured"
	fi
	if ! grep --silent "text.*does not include the latest version" "${logfile}"; then
		echo "‘outdated’ is expected to complain when the argument has an outdated dependency"
		cat "${logfile}"
	fi
}

main
