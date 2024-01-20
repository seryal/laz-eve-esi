{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazEveEsi;

{$warn 5023 off : no warning about unused units}
interface

uses
  esialliance, esiassets, esiauthorization, esibase, esibookmarks, 
  esicalendar, esicharacter, esiclones, esicontacts, esicontracts, 
  esihttpserver, esikillmails, esilocation, esiuniverse, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('LazEveEsi', @Register);
end.
