#!/usr/bin/env bats

app='./dist/build/outdated/outdated'

trace() {
  echo "$@" >/dev/tty
}

setup() {
  export OUTDATED_INDEX='https://hackage.haskell.org/packages/index.tar.gz'
  export OUTDATED_HOME=$(mktemp --directory)
}

teardown() {
  rm -r "$OUTDATED_HOME"
}

@test "The argument is a non-existing file" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/enoent.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : '.*openFile: does not exist.*'
}

@test "The argument is a random binary file" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/einval.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : '.*hGetContents: invalid argument.*'
}

@test "The argument is an invalid Cabal file" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/enoparse.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : ".*Couldn't parse.*${cabalfile}*"
}

@test "The argument is a valid Cabal file with no outdated dependencies" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/ok.cabal"
  [ "$status" -eq 0 ]
  expr "$output" : ".*OK.*"
}

@test "The argument is a valid Cabal file with a dependency that is not in the index" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/not-in-index.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : ".*unknown-dependency.*isn't in the index.*"
}

@test "The argument is a valid Cabal file with an outdated dependency" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/outdated.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : ".*text.*does not include the latest version.*"
}

@test "The arguments are multiple Cabal files with problems" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/"{not-in-index,outdated}'.cabal'
  [ "$status" -eq 1 ]
  expr "$output" : ".*unknown-dependency.*isn't in the index.*"
  expr "$output" : ".*text.*does not include the latest version.*"
}

@test "The argument is a valid Cabal file with an outdated dependency hidden behind an OS predicate" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/os.cabal"
  [ "$status" -eq 0 ]

  run "$app" --conf 'foo' "${BATS_TEST_DIRNAME}/asset/os.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : ".*text.*does not include the latest version.*"
}

@test "The argument is a valid Cabal file with an outdated dependency hidden behind an architecture predicate" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/arch.cabal"
  [ "$status" -eq 0 ]

  run "$app" --conf ';bar' "${BATS_TEST_DIRNAME}/asset/arch.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : ".*text.*does not include the latest version.*"
}

@test "The argument is a valid Cabal file with an outdated dependency hidden behind an implementation predicate" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/impl.cabal"
  [ "$status" -eq 0 ]

  run "$app" --conf ';;baz-3000' "${BATS_TEST_DIRNAME}/asset/impl.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : ".*text.*does not include the latest version.*"
}

@test "The argument is a valid Cabal file with an outdated dependency hidden behind a combination of flags" {
  run "$app" "${BATS_TEST_DIRNAME}/asset/flags.cabal"
  [ "$status" -eq 0 ]

  run "$app" --conf ';;;+qux,-quux' "${BATS_TEST_DIRNAME}/asset/flags.cabal"
  [ "$status" -eq 1 ]
  expr "$output" : ".*text.*does not include the latest version.*"
}
