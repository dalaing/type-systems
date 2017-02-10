{ mkDerivation, ansi-wl-pprint, base, bound, containers
, deriving-compat, equivalence, errors, lens, mtl, parsers, safe
, semigroupoids, semigroups, stdenv, text, transformers, trifecta
, unordered-containers
}:
mkDerivation {
  pname = "type-systems";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bound containers deriving-compat equivalence
    errors lens mtl parsers safe semigroupoids semigroups text
    transformers trifecta unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
