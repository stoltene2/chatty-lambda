{ mkDerivation, aeson, async, base, classy-prelude, hspec, HUnit
, network, QuickCheck, stdenv, stm, text, time, unix
}:
mkDerivation {
  pname = "chatty-lambda";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base classy-prelude network stm text time
  ];
  executableHaskellDepends = [ async base classy-prelude ];
  testHaskellDepends = [
    async base classy-prelude hspec HUnit network QuickCheck text unix
  ];
  homepage = "http://github.com/stoltene2/chatty-lambda";
  description = "Chat server for chatting with friends";
  license = stdenv.lib.licenses.bsd3;
}
