let 
    pkgs = import <nixpkgs> {}; 
    haskellPackages = pkgs.haskellPackages;
    cabal = haskellPackages.cabal;
in rec {
   decimal = cabal.mkDerivation(self : {
     pname = "Decimal";
     version = "0.3.1";
     sha256 = "12waaiis7z1isbyafjscnfiid6bsrfcgna0a2fa6gljalgnqzyb0";
     isLibrary = true;
     isExecutable = false;
     buildDepends = with haskellPackages; [ HUnit QuickCheck testFrameworkQuickcheck2 testFrameworkHunit ];
   }
   );
   mtgox = 
    cabal.mkDerivation (self: {
      pname = "MtgoxTrader";
      version = "0.0.1";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      buildDepends = with haskellPackages; [
        decimal aeson time SHA httpConduit network
      ];
  meta = {
    description = "A simple Mt.Gox trading bot";
    license = "WTFPL";
    platforms = self.ghc.meta.platforms;
    maintainers = [ ];
  };
});

}