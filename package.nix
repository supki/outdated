{ mkDerivation, base, bifunctors, bytestring, Cabal, conduit
, conduit-extra, containers, directory, filepath, hspec
, http-conduit, optparse-applicative, parsec, resourcet, semigroups
, stdenv, temporary, text, transformers
}:
mkDerivation {
  pname = "outdated";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bifunctors bytestring Cabal conduit conduit-extra containers
    directory filepath http-conduit optparse-applicative parsec
    resourcet semigroups temporary text transformers
  ];
  testHaskellDepends = [
    base bifunctors bytestring Cabal hspec optparse-applicative parsec
    semigroups transformers
  ];
  homepage = "http://supki.github.io/outdated";
  description = "Does your package accept the latest versions of its dependencies?";
  license = stdenv.lib.licenses.bsd2;
}
