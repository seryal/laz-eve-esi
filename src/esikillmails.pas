{==============================================================================|
| Project : EVE Online ESI Library                                             |
|==============================================================================|
| Copyright (c)2021, Yuri Serebrennikov                                        |
| All rights reserved.                                                         |
|==============================================================================|
| The Initial Developer of the Original Code is Yuri Serebrennikov             |
| All Rights Reserved.                                                         |
|==============================================================================|
|          (Found at URL: https://github.com/seryal/laz-eve-esi/)              |
|==============================================================================}
unit esikillmails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase;

type
  TEVESingleKillmailAttacker = class(TCollectionItem)
  private
    Falliance_id: Integer;
    Fcharacter_id: Integer;
    Fcorporation_id: Integer;
    Fdamage_done: Integer;
    Ffaction_id: Integer;
    Ffinal_blow: Boolean;
    Fsecurity_status: Double;
    Fship_type_id: Integer;
    Fweapon_type_id: Integer;
  published
    property alliance_id: Integer read Falliance_id write Falliance_id;
    property character_id: Integer read Fcharacter_id write Fcharacter_id;
    property corporation_id: Integer read Fcorporation_id write Fcorporation_id;
    property damage_done: Integer read Fdamage_done write Fdamage_done;
    property faction_id: Integer read Ffaction_id write Ffaction_id;
    property final_blow: Boolean read Ffinal_blow write Ffinal_blow;
    property security_status: Double read Fsecurity_status write Fsecurity_status;
    property ship_type_id: Integer read Fship_type_id write Fship_type_id;
    property weapon_type_id: Integer read Fweapon_type_id write Fweapon_type_id;
  end;

  TEVESingleKillmailAttackerList = class(TCollection)
  private
    function GetItems(Index: integer): TEVESingleKillmailAttacker;
  public
    property Items[Index: integer]: TEVESingleKillmailAttacker read GetItems;
  end;

  TEVESingleKillmailVictimItemsItem = class(TCollectionItem)
  private
    Fflag: Integer;
    Fitem_type_id: Integer;
    Fquantity_destroyed: Int64;
    Fquantity_dropped: Int64;
    Fsingleton: Integer;
  published
    property flag: Integer read Fflag write Fflag;
    property item_type_id: Integer read Fitem_type_id write Fitem_type_id;
    property quantity_destroyed: Int64 read Fquantity_destroyed write Fquantity_destroyed;
    property quantity_dropped: Int64 read Fquantity_dropped write Fquantity_dropped;
    property singleton: Integer read Fsingleton write Fsingleton;
  end;

  { TEVESingleKillmailVictimItemsItemList }

  TEVESingleKillmailVictimItemsItemList = class(TCollection)
  private
    function GetItems(Index: integer): TEVESingleKillmailVictimItemsItem;
  public
    property Items[Index: integer]: TEVESingleKillmailVictimItemsItem read GetItems;
  end;

  TEVESingleKillmailVictimItem = class(TEVESingleKillmailVictimItemsItem)
  private
    Fitems: TEVESingleKillmailVictimItemsItemList;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property items: TEVESingleKillmailVictimItemsItemList read Fitems write Fitems;
  end;

  { TEVESingleKillmailVictimItemList }

  TEVESingleKillmailVictimItemList = class(TCollection)
  private
    function GetItems(Index: integer): TEVESingleKillmailVictimItem;
  public
    property Items[Index: integer]: TEVESingleKillmailVictimItem read GetItems;
  end;

  TEVESingleKillmailPosition = class(TCollectionItem)
  private
    Fx: Double;
    Fy: Double;
    Fz: Double;
  published
    property x: Double read Fx write Fx;
    property y: Double read Fy write Fy;
    property z: Double read Fz write Fz;
  end;

  TEVESingleKillmailVictim = class(TCollectionItem)
  private
    Falliance_id: Integer;
    Fcharacter_id: Integer;
    Fcorporation_id: Integer;
    Fdamage_taken: Integer;
    Ffaction_id: Integer;
    Fitems: TEVESingleKillmailVictimItemList;
    Fposition: TEVESingleKillmailPosition;
    Fship_type_id: Integer;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property alliance_id: Integer read Falliance_id write Falliance_id;
    property character_id: Integer read Fcharacter_id write Fcharacter_id;
    property corporation_id: Integer read Fcorporation_id write Fcorporation_id;
    property damage_taken: Integer read Fdamage_taken write Fdamage_taken;
    property faction_id: Integer read Ffaction_id write Ffaction_id;
    property items: TEVESingleKillmailVictimItemList read FItems write FItems;
    property position: TEVESingleKillmailPosition read Fposition write Fposition;
    property ship_type_id: Integer read Fship_type_id write Fship_type_id;
  end;

  TEVESingleKillmail = class(TCollectionItem)
  private
    Fattackers: TEVESingleKillmailAttackerList;
    Fkillmail_id: Integer;
    Fkillmail_time: string;
    Fmoon_id: integer;
    Fsolar_system_id: Integer;
    Fvictim: TEVESingleKillmailVictim;
    Fwar_id: Integer;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property attackers: TEVESingleKillmailAttackerList read Fattackers write Fattackers;
    property killmail_id: Integer read Fkillmail_id write Fkillmail_id ;
    property killmail_time: string read Fkillmail_time write Fkillmail_time;
    property moon_id: integer read Fmoon_id write Fmoon_id;
    property solar_system_id: Integer read Fsolar_system_id write Fsolar_system_id;
    property victim: TEVESingleKillmailVictim read Fvictim write Fvictim;
    property war_id: Integer read Fwar_id write Fwar_id;
  end;

  TEVEKillmails = class(TESIBase)
  public
    {Return a single killmail from its ID and hash}
    function GetKillmails(AKillId: Integer; const AKillHash: string): TEVESingleKillmail;
  end;

implementation

{ TEVESingleKillmail }

constructor TEVESingleKillmail.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fattackers := TEVESingleKillmailAttackerList.Create(TEVESingleKillmailAttacker);
  Fvictim := TEVESingleKillmailVictim.Create(nil);
end;

destructor TEVESingleKillmail.Destroy;
begin
  FreeAndNil(Fattackers);
  FreeAndNil(Fvictim);
  inherited Destroy;
end;

{ TEVESingleKillmailAttackerList }

function TEVESingleKillmailAttackerList.GetItems(Index: integer): TEVESingleKillmailAttacker;
begin
  Result := TEVESingleKillmailAttacker(inherited Items[Index]);
end;

{ TEVESingleKillmailVictimItemsItemList }

function TEVESingleKillmailVictimItemsItemList.GetItems(Index: integer): TEVESingleKillmailVictimItemsItem;
begin
  Result := TEVESingleKillmailVictimItemsItem(inherited Items[Index]);
end;

{ TEVESingleKillmailVictimItem }

constructor TEVESingleKillmailVictimItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fitems := TEVESingleKillmailVictimItemsItemList.Create(TEVESingleKillmailVictimItemsItem);
end;

destructor TEVESingleKillmailVictimItem.Destroy;
begin
  FreeAndNil(Fitems);
  inherited Destroy;
end;

{ TEVESingleKillmailVictimItemList }

function TEVESingleKillmailVictimItemList.GetItems(Index: integer): TEVESingleKillmailVictimItem;
begin
  Result := TEVESingleKillmailVictimItem(inherited Items[Index]);
end;

constructor TEVESingleKillmailVictim.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Fitems := TEVESingleKillmailVictimItemList.Create(TEVESingleKillmailVictimItem);
  Fposition := TEVESingleKillmailPosition.Create(nil);
end;

destructor TEVESingleKillmailVictim.Destroy;
begin
  FreeAndNil(Fitems);
  FreeAndNil(Fposition);
  inherited Destroy;
end;

{ TEVEKillmails }

function TEVEKillmails.GetKillmails(AKillId: Integer; const AKillHash: string): TEVESingleKillmail;
const
  URL = 'https://esi.evetech.net/latest/killmails/%d/%s/?datasource=%s';
begin
  Result := TEVESingleKillmail.Create(nil);
  DeStreamerObject(Get(Format(URL, [AKillId, AKillHash, DataSource])), TObject(Result));
end;

end.

