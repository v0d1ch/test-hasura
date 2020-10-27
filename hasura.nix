{ mkDerivation, aeson, base, bytestring, containers, http-types
, mtl, stdenv, text, time, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "test-hasura";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers http-types mtl text time wai
    wai-websockets warp websockets
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
