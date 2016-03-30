{ mkDerivation, base, blaze-html, blaze-markup, bytestring
, containers, cryptonite, data-default, directory, doctest, hsass
, HUnit, lens, markdown, stdenv, test-framework
, test-framework-hunit, text, time, unix, validation
}:
mkDerivation {
  pname = "chris-martin-org";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base blaze-html blaze-markup bytestring containers cryptonite
    data-default directory hsass lens markdown text time unix
    validation
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base doctest HUnit test-framework test-framework-hunit
  ];
  license = stdenv.lib.licenses.asl20;
}
