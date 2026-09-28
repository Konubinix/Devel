# [[id:1b89d4e0-ce6a-4004-a8c9-14412280d36d][only the voyager should speak ergol:1]]
{ ... }:
{
  services.xserver.inputClassSections = [
    ''
      Identifier "voyager-ergol"
      MatchIsKeyboard "on"
      MatchUSBID "3297:1977"
      Option "XkbLayout" "fr"
      Option "XkbVariant" "ergol"
    ''
  ];
}
# only the voyager should speak ergol:1 ends here
