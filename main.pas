unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  esiauthorization, esihttpserver, LCLIntf, ComCtrls, ExtCtrls,
  JSONPropStorage, esicharacter,
  esilocation;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnBlueprints: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    btnLcation: TButton;
    btnAddScope: TButton;
    btnOnline: TButton;
    btnShip: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    edAuthCode: TEdit;
    edCharacterID: TEdit;
    edClientID: TEdit;
    edCallbackURL: TEdit;
    edScope: TEdit;
    edRefreshCode: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblName: TLabel;
    lblCharID: TLabel;
    lbScopes: TListBox;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    tbsCharacter: TTabSheet;
    tbsAuth: TTabSheet;
    tbsLocation: TTabSheet;
    procedure btnAddScopeClick(Sender: TObject);
    procedure btnBlueprintsClick(Sender: TObject);
    procedure btnOnlineClick(Sender: TObject);
    procedure btnShipClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btnLcationClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure edCallbackURLChange(Sender: TObject);
    procedure edClientIDChange(Sender: TObject);
    procedure edRefreshCodeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FUrl: string;
    FAuthCode: string;
    FCodeVerifier: string;
    FCodeChallenge: string;
    Fesi: TEVEESIAuth;
    Ftoken: TAccessToken;
    procedure TestError(Sender: TObject; E: Exception);
    procedure TestRequest(Sender: TObject; AResult, ACode, AState: string);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  str: string;
begin
  Fesi := TEVEESIAuth.Create;

  Fesi.ClientID := edClientID.Text;
  Fesi.CallbackURL := edCallbackURL.Text;
  for str in lbScopes.Items do
    Fesi.AddScope(str);
  Fesi.State := 'mystate';
  FUrl := Fesi.AuthURL;
  FCodeVerifier := Fesi.CodeVerifier;
  FCodeChallenge := Fesi.CodeChallenge;
  memo1.Lines.add(FUrl);

end;

procedure TForm1.btnAddScopeClick(Sender: TObject);
begin
  lbScopes.Items.Add(edScope.Text);
  JSONPropStorage1.StoredValue['scopes'] := lbScopes.Items.Text;
  JSONPropStorage1.Save;
end;

procedure TForm1.btnBlueprintsClick(Sender: TObject);
var
  character: TEVECharacter;
  res: TEVECharacterBlueprintList;
begin
  character := TEVECharacter.Create;
  try
    res := character.GetBlueprints(edAuthCode.Text, StrToInt(lblCharID.Caption), 1);
    try
      memo1.Lines.Add('Blueprints count = ' + res.Count.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.btnOnlineClick(Sender: TObject);
var
  esi: TEVEESILocation;
  online: TEVELocationOnline;
begin
  esi := TEVEESILocation.Create;
  try
    online := esi.GetOnline(edAuthCode.Text, StrToInt(lblCharID.Caption));
    try
      Memo1.Lines.Add('-----ONLINE------');
      Memo1.Lines.Add(online.last_login);
      Memo1.Lines.Add(online.last_logout);
      Memo1.Lines.Add(online.logins.ToString);
      Memo1.Lines.Add(online.online.ToString());
    finally
      FreeAndNil(online);
    end;
  finally
    FreeAndNil(esi);
  end;
end;

procedure TForm1.btnShipClick(Sender: TObject);
var
  esi: TEVEESILocation;
  ship: TEVELocationShip;
begin
  esi := TEVEESILocation.Create;
  try
    ship := esi.GetShip(edAuthCode.Text, StrToInt(lblCharID.Caption));
    try
      Memo1.Lines.Add('-----SHIP------');
      Memo1.Lines.Add(ship.ship_item_id.ToString);
      Memo1.Lines.Add(ship.ship_name);
      Memo1.Lines.Add(ship.ship_type_id.ToString);

    finally
      FreeAndNil(ship);
    end;
  finally
    FreeAndNil(esi);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  httpsrv: TEVEHTTPServer;
begin
  httpsrv := TEVEHTTPServer.Create(59020, '<b>TEST 2');
  httpsrv.OnRequest := @TestRequest;
  httpsrv.OnRequestError := @TestError;
  httpsrv.Start;
  Memo1.Lines.Add('++++++++++');
  memo1.Lines.Add(FUrl);
  Memo1.Lines.Add('++++++++++');
  OpenURL(FUrl);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Add('=====');
  Ftoken := Fesi.AuthByAccessToken(FAuthCode, FCodeChallenge);
  Memo1.Lines.Add(Ftoken.AccessToken);
  Memo1.Lines.Add(Ftoken.RefreshToken);
  Memo1.Lines.Add(Ftoken.TokenType);
  Memo1.Lines.Add(Ftoken.Expires.ToString);
  edRefreshCode.Text := Ftoken.RefreshToken;
  edAuthCode.Text := Ftoken.AccessToken;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  esi: TEVEESIAuth;
begin
  esi := TEVEESIAuth.Create;
  try
    esi.ClientID := edClientID.Text;
    //esi.AddScope('esi-location.read_location.v1');
    //esi.AddScope('esi-location.read_online.v1');
    //esi.AddScope('esi-location.read_ship_type.v1');
    Ftoken := esi.AuthByRefreshToken(edRefreshCode.Text);
    Memo1.Lines.Add('======== REFRESH ==================');
    Memo1.Lines.Add(Ftoken.AccessToken);
    Memo1.Lines.Add(Ftoken.RefreshToken);
    Memo1.Lines.Add(Ftoken.TokenType);
    Memo1.Lines.Add(Ftoken.Expires.ToString);
    edAuthCode.Text := Ftoken.AccessToken;
  finally
    FreeAndNil(esi);
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  esi: TEVEESIAuth;
  char: TCharacter;
begin
  esi := TEVEESIAuth.Create;
  try
    esi.ClientID := edClientID.Text;
    char := esi.GetCharacter(Ftoken.AccessToken);
    Memo1.Lines.Add('======== VERIFY ==================');
    char := esi.GetCharacter(Ftoken.AccessToken);
    Memo1.Lines.Add(char.CharacterName);
    lblName.Caption := char.CharacterName;
    lblCharID.Caption := char.CharacterID.ToString;
  finally
    FreeAndNil(esi);
  end;
end;

procedure TForm1.btnLcationClick(Sender: TObject);
var
  esi: TEVEESILocation;
  loc: TEVELocationLocation;
begin
  esi := TEVEESILocation.Create;
  try
    try
      loc := esi.GetLocation(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('-----LOCATION------');
      Memo1.Lines.Add(loc.solar_system_id.ToString);
      Memo1.Lines.Add(loc.station_id.ToString);
      Memo1.Lines.Add(loc.structure_id.ToString);
    finally
      FreeAndNil(loc);
    end;
  finally
    FreeAndNil(esi);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  character: TEVECharacter;
  res: TEVECharacterPublic;
begin
  character := TEVECharacter.Create;
  try
    res := character.GetPublicInfo(StrToInt(edCharacterID.Text));
    try
      memo1.Lines.Add(res.Name + ' - ' + res.Gender);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  character: TEVECharacter;
  res: TEVECharacterAgentList;
begin
  character := TEVECharacter.Create;
  try
    res := character.GetAgentResearch(edAuthCode.Text, StrToInt(lblCharID.Caption));
    try
      memo1.Lines.Add('Agents Count = ' + res.Count.ToString);
      memo1.Lines.Add(res.Items[0].started_at);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  character: TEVECharacter;
  res: TEVECharacterCorporationList;
begin
  character := TEVECharacter.Create;
  try
    res := character.GetCorporationHistory(StrToInt(edCharacterID.Text));
    try
      memo1.Lines.Add('Corporation Cont = ' + res.Count.ToString);
      memo1.Lines.Add(res.Items[0].corporation_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  character: TEVECharacter;
  res: TEVECharacterJumpFatigue;
begin
  character := TEVECharacter.Create;
  try
    res := character.GetJumpFatigue(edAuthCode.Text, StrToInt(lblCharID.Caption));
    try
      memo1.Lines.Add('Update = ' + res.jump_fatigue_expire_date);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.edCallbackURLChange(Sender: TObject);
begin
  JSONPropStorage1.StoredValue['callbackurl'] := edCallbackURL.Text;
  JSONPropStorage1.Save;
end;

procedure TForm1.edClientIDChange(Sender: TObject);
begin
  JSONPropStorage1.StoredValue['clientid'] := edClientID.Text;
  JSONPropStorage1.Save;
end;

procedure TForm1.edRefreshCodeChange(Sender: TObject);
begin
  JSONPropStorage1.StoredValue['refresh_code'] := edRefreshCode.Text;
  JSONPropStorage1.Save;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(Fesi);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  JSONPropStorage1.Restore;
  edClientID.Text := JSONPropStorage1.StoredValue['clientid'];
  edCallbackURL.Text := JSONPropStorage1.StoredValue['callbackurl'];
  edRefreshCode.Text := JSONPropStorage1.StoredValue['refresh_code'];
  lbScopes.Items.Text := JSONPropStorage1.StoredValue['scopes'];
end;

procedure TForm1.TestRequest(Sender: TObject; AResult, ACode, AState: string);
begin
  FAuthCode := ACode;
  Memo1.Lines.Add('-----');
  Memo1.Lines.Add(AResult);
  Memo1.Lines.Add(ACode);
  Memo1.Lines.Add(AState);
  TEVEHTTPServer(Sender).StopAndFree;
  FAuthCode := ACode;
end;

procedure TForm1.TestError(Sender: TObject; E: Exception);
begin
  memo1.Lines.Add(E.Message);
end;

end.
