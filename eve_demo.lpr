program eve_demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, ESIAuthorization, esihttpserver,
  esilocation, esibase, esicharacter, esialliance, esiassets, esibookmarks,
  esicalendar, esiclones, esicontacts, esicontracts, esikillmails, esiuniverse
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

