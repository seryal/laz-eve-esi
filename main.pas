unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  esiauthorization, esihttpserver, LCLIntf, ComCtrls, ExtCtrls,
  JSONPropStorage, esicharacter, esialliance,
  esilocation, esiassets, esibookmarks, esicalendar, esiclones, esicontacts, esicontracts, esikillmails, esiuniverse;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAffilation: TButton;
    btnBlueprints: TButton;
    Button1: TButton;
    btnMedals: TButton;
    btnNotification: TButton;
    btnContacts: TButton;
    Button10: TButton;
    btnRoles: TButton;
    btnCharFolders: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
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
    btnCorporation: TButton;
    btnStanding: TButton;
    btnTitle: TButton;
    btnAllianceList: TButton;
    btnInfo: TButton;
    btnAllianceCorporation: TButton;
    btnAllianceIcon: TButton;
    btnAssets: TButton;
    btnAssetsLocation: TButton;
    btnAssetNames: TButton;
    btnCorporationAssets: TButton;
    btnCorpLocation: TButton;
    btnCorpNames: TButton;
    btnCharBookmark: TButton;
    btnCalendarResponse: TButton;
    btnClones: TButton;
    btnImplants: TButton;
    btnContactCharacter: TButton;
    Button8: TButton;
    Button9: TButton;
    edAuthCode: TEdit;
    edCharacterID: TEdit;
    edClientID: TEdit;
    edCallbackURL: TEdit;
    edAllianceID: TEdit;
    edUniverseCategoryId: TEdit;
    edUniverseGroupId: TEdit;
    edUniverseTypeId: TEdit;
    edKillId: TEdit;
    edKillHash: TEdit;
    edScope: TEdit;
    edRefreshCode: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    JSONPropStorage1: TJSONPropStorage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblCorpId: TLabel;
    lblName: TLabel;
    lblCharID: TLabel;
    lbScopes: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tbsContracts: TTabSheet;
    tbsContact: TTabSheet;
    tbsClones: TTabSheet;
    tbsCalendar: TTabSheet;
    tbsBookmark: TTabSheet;
    tbsAssets: TTabSheet;
    tbsAlliance: TTabSheet;
    tbsCharacter: TTabSheet;
    tbsAuth: TTabSheet;
    tbsLocation: TTabSheet;
    procedure btnAddScopeClick(Sender: TObject);
    procedure btnAffilationClick(Sender: TObject);
    procedure btnAllianceCorporationClick(Sender: TObject);
    procedure btnAllianceIconClick(Sender: TObject);
    procedure btnAllianceListClick(Sender: TObject);
    procedure btnAssetNamesClick(Sender: TObject);
    procedure btnAssetsClick(Sender: TObject);
    procedure btnAssetsLocationClick(Sender: TObject);
    procedure btnBlueprintsClick(Sender: TObject);
    procedure btnCalendarResponseClick(Sender: TObject);
    procedure btnCharBookmarkClick(Sender: TObject);
    procedure btnCharFoldersClick(Sender: TObject);
    procedure btnClonesClick(Sender: TObject);
    procedure btnContactCharacterClick(Sender: TObject);
    procedure btnContactsClick(Sender: TObject);
    procedure btnCorpLocationClick(Sender: TObject);
    procedure btnCorpNamesClick(Sender: TObject);
    procedure btnCorporationAssetsClick(Sender: TObject);
    procedure btnImplantsClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnMedalsClick(Sender: TObject);
    procedure btnNotificationClick(Sender: TObject);
    procedure btnOnlineClick(Sender: TObject);
    procedure btnRolesClick(Sender: TObject);
    procedure btnShipClick(Sender: TObject);
    procedure btnStandingClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btnLcationClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure btnCorporationClick(Sender: TObject);
    procedure btnTitleClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure edCallbackURLChange(Sender: TObject);
    procedure edClientIDChange(Sender: TObject);
    procedure edRefreshCodeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
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

procedure TForm1.btnAffilationClick(Sender: TObject);
var
  tmp: TESICharacter;
  res: TEVECharacterAffiliationList;
  str: TStringList;
begin
  tmp := TESICharacter.Create;
  try
    try
      str := TStringList.Create;
      str.Add('93153521');
      str.Add('96614010');
      res := tmp.GetAffiliation(str);
      Memo1.Lines.Add('-----AFFILATION------');
      Memo1.Lines.Add(res.Items[0].corporation_id.ToString);
      FreeAndNil(str);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.btnAllianceCorporationClick(Sender: TObject);
var
  tmp: TESIAlliance;
  res: TEVEAllianceCorporationList;
begin
  tmp := TESIAlliance.Create;
  try
    try
      res := tmp.GetCorporations(StrToInt(edAllianceID.Text));
      Memo1.Lines.Add('-----CORPORATIONS------');
      Memo1.Lines.Add(res.Count.ToString);
      Memo1.Lines.Add(res.Items[0].corporation_id.ToString);
      Memo1.Lines.Add(res.Items[1].corporation_id.ToString);
      Memo1.Lines.Add(res.Items[2].corporation_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnAllianceIconClick(Sender: TObject);
var
  tmp: TESIAlliance;
  res: TEVEAllianceIcons;
begin
  tmp := TESIAlliance.Create;
  try
    try
      res := tmp.GetIcon(StrToInt(edAllianceID.Text));
      Memo1.Lines.Add('-----ICONS------');
      Memo1.Lines.Add(res.px128x128);
      Memo1.Lines.Add(res.px64x64);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnAllianceListClick(Sender: TObject);
var
  tmp: TESIAlliance;
  res: TEVEAllianceList;
begin
  tmp := TESIAlliance.Create;
  try
    try
      res := tmp.GetAlliances;
      Memo1.Lines.Add('-----ALLIANCES------');
      Memo1.Lines.Add(res.Count.ToString);
      Memo1.Lines.Add(res.Items[0].alliance_id.ToString);
      Memo1.Lines.Add(res.Items[1].alliance_id.ToString);
      Memo1.Lines.Add(res.Items[2].alliance_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnAssetNamesClick(Sender: TObject);
var
  tmp: TESIAssets;
  res: TEVEAssetNameList;
  list: TStringList;
begin
  tmp := TESIAssets.Create;
  try
    try
      list := TStringList.Create;
      list.Add('34');
      list.Add('35');
      list.Add('36');
      res := tmp.GetNames(edAuthCode.Text, StrToInt(lblCharID.Caption), list);
      FreeAndNil(list);
      Memo1.Lines.Add('-----LOCATION------');
      Memo1.Lines.Add(res.Items[0].item_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnAssetsClick(Sender: TObject);
var
  tmp: TESIAssets;
  res: TEVEAssetCharacterList;
  Count: integer;
  page: integer;
begin
  tmp := TESIAssets.Create;
  try
    try
      Count := 1000;
      page := 1;
      while Count <> 0 do
      begin
        res := tmp.GetAssets(edAuthCode.Text, StrToInt(lblCharID.Caption), page);
        Count := res.Count;
        FreeAndNil(res);
        memo1.Lines.Add('Assets count = ' + Count.ToString);
        Inc(page);
        if Count < 1000 then break;
      end;
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnAssetsLocationClick(Sender: TObject);
var
  tmp: TESIAssets;
  res: TEVEAssetLocationList;
  list: TStringList;
begin
  tmp := TESIAssets.Create;
  try
    try
      list := TStringList.Create;
      list.Add('34');
      list.Add('35');
      list.Add('36');
      res := tmp.GetLocation(edAuthCode.Text, StrToInt(lblCharID.Caption), list);
      FreeAndNil(list);
      Memo1.Lines.Add('-----LOCATION------');
      Memo1.Lines.Add(res.Items[0].item_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnBlueprintsClick(Sender: TObject);
var
  character: TESICharacter;
  res: TEVECharacterBlueprintList;
begin
  character := TESICharacter.Create;
  try
    try
      res := character.GetBlueprints(edAuthCode.Text, StrToInt(lblCharID.Caption), 1);
      memo1.Lines.Add('Blueprints count = ' + res.Count.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.btnCalendarResponseClick(Sender: TObject);
var
  tmp: TESICalendar;
  res: boolean;
begin
  tmp := TESICalendar.Create;
  try
    res := tmp.SetResponse(edAuthCode.Text, StrToInt(lblCharID.Caption), 2452800, rspAccepted);
    //memo1.Lines.Add('Blueprints count = ' + res.Count.ToString);
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnCharBookmarkClick(Sender: TObject);

var
  tmp: TESIBookmarks;
  res: TEVEBookmarkList;
begin
  tmp := TESIBookmarks.Create;
  try
    try
      res := tmp.GetCharacterBookmarks(edAuthCode.Text, StrToInt(lblCharID.Caption), 1);
      Memo1.Lines.Add('-----CHAR BOOKMARKS------');
      Memo1.Lines.Add(res.Count.ToString);
      Memo1.Lines.Add(res.Items[0].&label);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.btnCharFoldersClick(Sender: TObject);
var
  tmp: TESIBookmarks;
  res: TEVEBookmarksFolderList;
begin
  tmp := TESIBookmarks.Create;
  try
    try
      res := tmp.GetCharacterBookmarksFolders(edAuthCode.Text, StrToInt(lblCharID.Caption), 1);
      Memo1.Lines.Add('-----CHAR FOLDERS------');
      Memo1.Lines.Add(res.Count.ToString);
      Memo1.Lines.Add(res.Items[0].Name);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.btnClonesClick(Sender: TObject);
var
  tmp: TESIClones;
  res: TEVEClonesCharacter;
begin
  tmp := TESIClones.Create;
  try
    try
      res := tmp.GetClones(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('------CLONES-------');
      memo1.Lines.Add(res.home_location.location_id.ToString);
      memo1.Lines.Add(res.home_location.location_type);

      //memo1.Lines.Add(res.jump_clones.Items[0].implants.Items[0].ToString);

    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.btnContactCharacterClick(Sender: TObject);
var
  tmp: TESIContacts;
  res: TEVEContactCharacterList;
  i: integer;
begin
  tmp := TESIContacts.Create;
  try
    try
      res := tmp.GetCharacterContacts(edAuthCode.Text, StrToInt(lblCharID.Caption), 1);
      Memo1.Lines.Add('------CONTACT CHAR-------');
      for i := 0 to res.Count - 1 do
      begin
        memo1.Lines.Add(res.Items[i].contact_id.ToString);
        memo1.Lines.Add(res.Items[i].label_ids.Count.ToString);
      end;
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.btnContactsClick(Sender: TObject);
var
  character: TESICharacter;
  res: TEVECharacterContactList;
begin
  character := TESICharacter.Create;
  try
    try
      res := character.GetContacts(edAuthCode.Text, StrToInt(lblCharID.Caption));
      memo1.Lines.Add('Count = ' + res.Count.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.btnCorpLocationClick(Sender: TObject);
var
  tmp: TESIAssets;
  res: TEVEAssetLocationList;
  list: TStringList;
begin
  tmp := TESIAssets.Create;
  try
    try
      list := TStringList.Create;
      list.Add('34');
      list.Add('35');
      list.Add('36');
      res := tmp.GetCorporationLocation(edAuthCode.Text, StrToInt(lblCorpId.Caption), list);
      FreeAndNil(list);
      Memo1.Lines.Add('-----LOCATION------');
      Memo1.Lines.Add(res.Items[0].item_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnCorpNamesClick(Sender: TObject);
var
  tmp: TESIAssets;
  res: TEVEAssetNameList;
  list: TStringList;
begin
  tmp := TESIAssets.Create;
  try
    try
      list := TStringList.Create;
      list.Add('34');
      list.Add('35');
      list.Add('36');
      res := tmp.GetCorporationNames(edAuthCode.Text, StrToInt(lblCharID.Caption), list);
      FreeAndNil(list);
      Memo1.Lines.Add('-----LOCATION------');
      Memo1.Lines.Add(res.Items[0].item_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnCorporationAssetsClick(Sender: TObject);
var
  tmp: TESIAssets;
  res: TEVEAssetCorporationList;
  Count: integer;
  page: integer;
begin
  tmp := TESIAssets.Create;
  try
    try
      Count := 1000;
      page := 1;
      while Count <> 0 do
      begin
        res := tmp.GetCorporation(edAuthCode.Text, StrToInt(lblCorpId.Caption), page);
        Count := res.Count;
        FreeAndNil(res);
        memo1.Lines.Add('Assets count = ' + Count.ToString);
        Inc(page);
        if Count < 1000 then break;
      end;
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnImplantsClick(Sender: TObject);
var
  tmp: TESIClones;
  res: TEVEImplantList;
begin
  tmp := TESIClones.Create;
  try
    try
      res := tmp.GetImplants(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('------IMPLANTS-------');
      memo1.Lines.Add(res.Count.ToString);
      memo1.Lines.Add(res.Items[0].ToString);

      //memo1.Lines.Add(res.jump_clones.Items[0].implants.Items[0].ToString);

    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.btnInfoClick(Sender: TObject);
var
  tmp: TESIAlliance;
  res: TEVEAllianceInfo;
begin
  tmp := TESIAlliance.Create;
  try
    try
      res := tmp.GetInfo(StrToInt(edAllianceID.Text));
      Memo1.Lines.Add('-----ALLIANCES------');
      Memo1.Lines.Add(res.&name);
      Memo1.Lines.Add(res.date_founded);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnMedalsClick(Sender: TObject);
var
  tmp: TESICharacter;
  medals: TEVECharacterMedalsList;
begin
  tmp := TESICharacter.Create;
  try
    try
      medals := tmp.GetMedals(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('-----MEDALS------');
      Memo1.Lines.Add(medals.Items[0].description);
      Memo1.Lines.Add(medals.Items[0].Graphics.Items[0].graphic);
    finally
      FreeAndNil(medals);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnNotificationClick(Sender: TObject);
var
  tmp: TESICharacter;
  res: TEVECharacterNotificationList;
begin
  tmp := TESICharacter.Create;
  try
    try
      res := tmp.GetNotifications(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('-----NOTIFICATIONS------');
      Memo1.Lines.Add(res.Items[0].sender_id.ToString);
      Memo1.Lines.Add(res.Items[0].Text);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.btnOnlineClick(Sender: TObject);
var
  esi: TESILocation;
  online: TEVELocationOnline;
begin
  esi := TESILocation.Create;
  try
    try
      online := esi.GetOnline(edAuthCode.Text, StrToInt(lblCharID.Caption));
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

procedure TForm1.btnRolesClick(Sender: TObject);
var
  tmp: TESICharacter;
  res: TEVECharacterRoles;
begin
  tmp := TESICharacter.Create;
  try
    try
      res := tmp.GetRoles(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('-----Roles------');
      Memo1.Lines.Add(res.roles.Text);
      Memo1.Lines.Add(res.roles_at_base.Text);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.btnShipClick(Sender: TObject);
var
  esi: TESILocation;
  ship: TEVELocationShip;
begin
  esi := TESILocation.Create;
  try
    try
      ship := esi.GetShip(edAuthCode.Text, StrToInt(lblCharID.Caption));
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

procedure TForm1.btnStandingClick(Sender: TObject);
var
  esi: TESICharacter;
  tmp: TEVECharacterStandingList;
begin
  esi := TESICharacter.Create;
  try
    try
      tmp := esi.GetStanding(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('-----STANDING------');
      Memo1.Lines.Add(tmp.Items[0].from_type);

    finally
      FreeAndNil(tmp);
    end;
  finally
    FreeAndNil(esi);
  end;
end;

procedure TForm1.Button10Click(Sender: TObject);
var
  esi: TESICharacter;
  tmp: TEVECharacterPortrait;
begin
  esi := TESICharacter.Create;
  try
    try
      tmp := esi.GetPortrait(StrToInt(edCharacterID.Text));
      Memo1.Lines.Add('-----PORTRAIT------');
      Memo1.Lines.Add(tmp.px128x128);

    finally
      FreeAndNil(tmp);
    end;
  finally
    FreeAndNil(esi);
  end;

end;

procedure TForm1.Button11Click(Sender: TObject);
var
  Killmail: TEVEKillmails;
  SingleKillmail: TEVESingleKillmail;
  l: TEVESingleKillmailVictim;
  I: integer;
  id: Integer;

begin
  Killmail := TEVEKillmails.Create;
  try
    SingleKillmail := Killmail.GetKillMails(StrToInt(edKillId.Text), edKillHash.Text);
    I := SingleKillmail.attackers.Count;
    l := SingleKillmail.victim;
    Memo1.Lines.Add('CorpId: ' + l.items.Items[0].item_type_id.ToString);
    Memo1.Lines.Add('Position X: ' + l.position.x.ToString);
  finally
    Killmail.Free;
  end;
  //
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  EVEUniverse: TEVEUniverse;
  EVEUniverseTypesType: TEVEUniverseTypesType;
  Str: String;
begin
  EVEUniverse := nil;
  EVEUniverseTypesType := nil;
  try
    EVEUniverse := TEVEUniverse.Create;
    EVEUniverseTypesType := EVEUniverse.GetUniverseTypesType(StrToInt(edUniverseTypeId.Text));
    Str := EVEUniverseTypesType.name;
    Memo1.Lines.Add('Type Name: ' + Str);
  finally
    EVEUniverseTypesType.Free;
    EVEUniverse.Free;
  end;
end;

procedure TForm1.Button13Click(Sender: TObject);
var
  EVEUniverse: TEVEUniverse;
  EVEUniverseCategoriesCategory: TEVEUniverseCategoriesCategory;
  Str: String;
begin
  EVEUniverse := nil;
  EVEUniverseCategoriesCategory := nil;
  try
    EVEUniverse := TEVEUniverse.Create;
    EVEUniverseCategoriesCategory := EVEUniverse.GetUniverseCategoriesCategory(StrToInt(edUniverseCategoryId.Text));
    Str := EVEUniverseCategoriesCategory.name;
    Memo1.Lines.Add('Category Name: ' + Str);
  finally
    EVEUniverseCategoriesCategory.Free;
    EVEUniverse.Free;
  end;
end;

procedure TForm1.Button14Click(Sender: TObject);
var
  EVEUniverse: TEVEUniverse;
  EVEUniverseGroupsGroup: TEVEUniverseGroupsGroup;
  Str: String;
begin
  EVEUniverse := nil;
  EVEUniverseGroupsGroup := nil;
  try
    EVEUniverse := TEVEUniverse.Create;
    EVEUniverseGroupsGroup := EVEUniverse.GetUniverseGroupsGroup(StrToInt(edUniverseGroupId.Text));
    Str := EVEUniverseGroupsGroup.name;
    Memo1.Lines.Add('Group Name: ' + Str);
  finally
    EVEUniverseGroupsGroup.Free;
    EVEUniverse.Free;
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
  char: TEVEAuthCharacter;
  info: TESICharacter;
  val: TEVECharacterPublic;
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
    info := TESICharacter.Create;
    val := info.GetPublicInfo(StrToInt(lblCharID.Caption));
    lblCorpId.Caption := val.corporation_id.ToString;
    FreeAndNil(val);
    FreeAndNil(info);
  finally
    FreeAndNil(esi);
  end;
end;

procedure TForm1.btnLcationClick(Sender: TObject);
var
  esi: TESILocation;
  loc: TEVELocationLocation;
begin
  esi := TESILocation.Create;
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
  character: TESICharacter;
  res: TEVECharacterPublic;
begin
  character := TESICharacter.Create;
  try
    try
      res := character.GetPublicInfo(StrToInt(edCharacterID.Text));
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
  character: TESICharacter;
  res: TEVECharacterAgentList;
begin
  character := TESICharacter.Create;
  try
    try
      res := character.GetAgentResearch(edAuthCode.Text, StrToInt(lblCharID.Caption));
      memo1.Lines.Add('Agents Count = ' + res.Count.ToString);
      memo1.Lines.Add(res.Items[0].started_at);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.btnCorporationClick(Sender: TObject);
var
  character: TESICharacter;
  res: TEVECharacterCorporationList;
begin
  character := TESICharacter.Create;
  try
    try
      res := character.GetCorporationHistory(StrToInt(edCharacterID.Text));
      memo1.Lines.Add('Corporation Cont = ' + res.Count.ToString);
      //     memo1.Lines.Add(res.Items[0].corporation_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(character);
  end;
end;

procedure TForm1.btnTitleClick(Sender: TObject);
var
  tmp: TESICharacter;
  res: TEVECharacterTitleList;
begin
  tmp := TESICharacter.Create;
  try
    try
      res := tmp.GetTitles(edAuthCode.Text, StrToInt(lblCharID.Caption));
      Memo1.Lines.Add('-----TITLE------');
      Memo1.Lines.Add(res.Items[0].title_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;

end;

procedure TForm1.Button8Click(Sender: TObject);
var
  tmp: TESIContracts;
  res: TEVEContractList;
begin
  tmp := TESIContracts.Create;
  try
    try
      res := tmp.GetPublicContracts(10000002, 1);
      Memo1.Lines.Add('-----CONTRACTS PUBLIC------');
      Memo1.Lines.Add(res.Items[0].issuer_id.ToString);
    finally
      FreeAndNil(res);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  character: TESICharacter;
  res: TEVECharacterJumpFatigue;
begin
  character := TESICharacter.Create;
  try
    try
      res := character.GetJumpFatigue(edAuthCode.Text, StrToInt(lblCharID.Caption));
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

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin

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
