{ cabal, lens, safe, strptime, time }:

cabal.mkDerivation (self: {
  pname = "karamaan-plankton";
  version = "0.2";
  src = ./.;
  buildDepends = [ lens safe strptime time ];
  doCheck = false;
  meta = {
    description = "A small utility package for Opaleye";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
