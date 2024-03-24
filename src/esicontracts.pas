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
unit esicontracts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase;

type

  { TEVEContract }

  TEVEContract = class(TCollectionItem)
  private
    Fbuyout: double;
    Fcollateral: double;
    Fcontract_id: integer;
    Fdate_expired: string;
    Fdate_issued: string;
    Fdays_to_complete: integer;
    Fend_location_id: uint64;
    Ffor_corporation: boolean;
    Fissuer_corporation_id: integer;
    Fissuer_id: integer;
    Fprice: double;
    Freward: double;
    Fstart_location_id: uint64;
    Ftitle: string;
    Ftype: string;
    Fvolume: double;
  published
    property buyout: double read Fbuyout write Fbuyout;
    property collateral: double read Fcollateral write Fcollateral;
    property contract_id: integer read Fcontract_id write Fcontract_id;
    property date_expired: string read Fdate_expired write Fdate_expired;
    property date_issued: string read Fdate_issued write Fdate_issued;
    property days_to_complete: integer read Fdays_to_complete write Fdays_to_complete;
    property end_location_id: uint64 read Fend_location_id write Fend_location_id;
    property for_corporation: boolean read Ffor_corporation write Ffor_corporation;
    property issuer_corporation_id: integer read Fissuer_corporation_id write Fissuer_corporation_id;
    property issuer_id: integer read Fissuer_id write Fissuer_id;
    property price: double read Fprice write Fprice;
    property reward: double read Freward write Freward;
    property start_location_id: uint64 read Fstart_location_id write Fstart_location_id;
    property title: string read Ftitle write Ftitle;
    property &type: string read Ftype write Ftype;
    property volume: double read Fvolume write Fvolume;
  end;

  { TEVEContractList }

  TEVEContractList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContract;
  public
    property Items[Index: integer]: TEVEContract read GetItems;
  end;

  { TEVEContractBid }

  TEVEContractBid = class(TCollectionItem)
  private
    Famount: double;
    Fbid_id: integer;
    Fdate_bid: string;
  published
    property amount: double read Famount write Famount;
    property bid_id: integer read Fbid_id write Fbid_id;
    property date_bid: string read Fdate_bid write Fdate_bid;
  end;

  { TEVEContractBidList }

  TEVEContractBidList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractBid;
  public
    property Items[Index: integer]: TEVEContractBid read GetItems;
  end;

  { TEVEContractItem }

  TEVEContractItem = class(TCollectionItem)
  private
    Fis_blueprint_copy: boolean;
    Fis_included: boolean;
    Fitem_id: uint64;
    Fmaterial_efficiency: integer;
    Fquantity: integer;
    Frecord_id: uint64;
    Fruns: integer;
    Ftime_efficiency: integer;
    Ftype_id: integer;
  published
    property is_blueprint_copy: boolean read Fis_blueprint_copy write Fis_blueprint_copy;
    property is_included: boolean read Fis_included write Fis_included;
    property item_id: uint64 read Fitem_id write Fitem_id;
    property material_efficiency: integer read Fmaterial_efficiency write Fmaterial_efficiency;
    property quantity: integer read Fquantity write Fquantity;
    property record_id: uint64 read Frecord_id write Frecord_id;
    property runs: integer read Fruns write Fruns;
    property time_efficiency: integer read Ftime_efficiency write Ftime_efficiency;
    property type_id: integer read Ftype_id write Ftype_id;
  end;

  { TEVEContractItemList }

  TEVEContractItemList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractItem;
  public
    property Items[Index: integer]: TEVEContractItem read GetItems;
  end;

  { TEVEContractCharacter }

  TEVEContractCharacter = class(TEVEContract)
  private
    Facceptor_id: integer;
    Fassignee_id: integer;
    Favailability: string;
    Fdate_accepted: string;
    Fdate_completed: string;
    Fstatus: string;
  published
    property acceptor_id: integer read Facceptor_id write Facceptor_id;
    property assignee_id: integer read Fassignee_id write Fassignee_id;
    property availability: string read Favailability write Favailability;
    property buyout: double read Fbuyout write Fbuyout;
    property collateral: double read Fcollateral write Fcollateral;
    property contract_id: integer read Fcontract_id write Fcontract_id;
    property date_accepted: string read Fdate_accepted write Fdate_accepted;
    property date_completed: string read Fdate_completed write Fdate_completed;
    property date_expired: string read Fdate_expired write Fdate_expired;
    property date_issued: string read Fdate_issued write Fdate_issued;
    property days_to_complete: integer read Fdays_to_complete write Fdays_to_complete;
    property end_location_id: uint64 read Fend_location_id write Fend_location_id;
    property for_corporation: boolean read Ffor_corporation write Ffor_corporation;
    property issuer_corporation_id: integer read Fissuer_corporation_id write Fissuer_corporation_id;
    property issuer_id: integer read Fissuer_id write Fissuer_id;
    property price: double read Fprice write Fprice;
    property reward: double read Freward write Freward;
    property start_location_id: uint64 read Fstart_location_id write Fstart_location_id;
    property status: string read Fstatus write Fstatus;
    property title: string read Ftitle write Ftitle;
    property &type: string read Ftype write Ftype;
    property volume: double read Fvolume write Fvolume;
  end;

  { TEVEContractCharacterList }

  TEVEContractCharacterList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractCharacter;
  public
    property Items[Index: integer]: TEVEContractCharacter read GetItems;
  end;

  { TEVEContractCharacterBid }

  TEVEContractCharacterBid = class(TEVEContractBid)
  private
    Fbidder_id: integer;
  published
    property amount: double read Famount write Famount;
    property bid_id: integer read Fbid_id write Fbid_id;
    property bidder_id: integer read Fbidder_id write Fbidder_id;
    property date_bid: string read Fdate_bid write Fdate_bid;
  end;

  { TEVEContractCharacterBidList }

  TEVEContractCharacterBidList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractCharacterBid;
  public
    property Items[Index: integer]: TEVEContractCharacterBid read GetItems;
  end;

  { TEVEContractCharacterItem }

  TEVEContractCharacterItem = class(TEVEContractItem)
  private
    Fis_singleton: boolean;
    Fraw_quantity: integer;
  published
    property is_included: boolean read Fis_included write Fis_included;
    property is_singleton: boolean read Fis_singleton write Fis_singleton;
    property quantity: integer read Fquantity write Fquantity;
    property raw_quantity: integer read Fraw_quantity write Fraw_quantity;
    property record_id: uint64 read Frecord_id write Frecord_id;
    property type_id: integer read Ftype_id write Ftype_id;
  end;

  { TEVEContractCharacterItemList }

  TEVEContractCharacterItemList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractCharacterItem;
  public
    property Items[Index: integer]: TEVEContractCharacterItem read GetItems;
  end;

  TEVEContractCorporation = class(TEVEContractCharacter)
  published
    property acceptor_id: integer read Facceptor_id write Facceptor_id;
    property assignee_id: integer read Fassignee_id write Fassignee_id;
    property availability: string read Favailability write Favailability;
    property buyout: double read Fbuyout write Fbuyout;
    property collateral: double read Fcollateral write Fcollateral;
    property contract_id: integer read Fcontract_id write Fcontract_id;
    property date_accepted: string read Fdate_accepted write Fdate_accepted;
    property date_completed: string read Fdate_completed write Fdate_completed;
    property date_expired: string read Fdate_expired write Fdate_expired;
    property date_issued: string read Fdate_issued write Fdate_issued;
    property days_to_complete: integer read Fdays_to_complete write Fdays_to_complete;
    property end_location_id: uint64 read Fend_location_id write Fend_location_id;
    property for_corporation: boolean read Ffor_corporation write Ffor_corporation;
    property issuer_corporation_id: integer read Fissuer_corporation_id write Fissuer_corporation_id;
    property issuer_id: integer read Fissuer_id write Fissuer_id;
    property price: double read Fprice write Fprice;
    property reward: double read Freward write Freward;
    property start_location_id: uint64 read Fstart_location_id write Fstart_location_id;
    property status: string read Fstatus write Fstatus;
    property title: string read Ftitle write Ftitle;
    property &type: string read Ftype write Ftype;
    property volume: double read Fvolume write Fvolume;
  end;

  { TEVEContractCorporationList }

  TEVEContractCorporationList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractCorporation;
  public
    property Items[Index: integer]: TEVEContractCorporation read GetItems;
  end;

  { TEVEContractCorporationBid }

  TEVEContractCorporationBid = class(TEVEContractCharacterBid)
  published
    property amount: double read Famount write Famount;
    property bid_id: integer read Fbid_id write Fbid_id;
    property bidder_id: integer read Fbidder_id write Fbidder_id;
    property date_bid: string read Fdate_bid write Fdate_bid;
  end;

  { TEVEContractCorporationBidList }

  TEVEContractCorporationBidList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractCorporationBid;
  public
    property Items[Index: integer]: TEVEContractCorporationBid read GetItems;
  end;

  { TEVEContractCorporationItem }

  TEVEContractCorporationItem = class(TEVEContractCharacterItem)
  published
    property is_included: boolean read Fis_included write Fis_included;
    property is_singleton: boolean read Fis_singleton write Fis_singleton;
    property quantity: integer read Fquantity write Fquantity;
    property raw_quantity: integer read Fraw_quantity write Fraw_quantity;
    property record_id: uint64 read Frecord_id write Frecord_id;
    property type_id: integer read Ftype_id write Ftype_id;
  end;

  { TEVEContractCorporationItemList }

  TEVEContractCorporationItemList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEContractCorporationItem;
  public
    property Items[Index: integer]: TEVEContractCorporationItem read GetItems;
  end;

  { TESIContracts }

  TESIContracts = class(TESIBase)
  public
    function GetContractsCharacter(const AAccessToken: string; ACharacterId: integer; APage: integer): string;
    function GetContractCharactersBids(const AAccessToken: string; ACharacterId, AContractID: integer): string;
    function GetContractCharactersItems(const AAccessToken: string; ACharacterId, AContractID, APage: integer): string;
    function GetPublicContracts(ARegionID, APage: integer): TEVEContractList;
    function GetPublicBids(AContractID: integer): string;
    function GetPublicItems(AContractID, APage: integer): string;
    function GetContractsCorporation(const AAccessToken: string; ACharacterId: integer; APage: integer): string;
    function GetContractCorporationBids(const AAccessToken: string; ACorporationId, AContractID: integer): string;
    function GetContractCorporationItems(const AAccessToken: string; ACorporationId, AContractID, APage: integer): string;
  end;

implementation

{ TEVEContractCorporationBidList }

function TEVEContractCorporationBidList.GetItems(Index: integer): TEVEContractCorporationBid;
begin
  Result := TEVEContractCorporationBid(inherited Items[Index]);
end;

{ TEVEContractCorporationItemList }

function TEVEContractCorporationItemList.GetItems(Index: integer): TEVEContractCorporationItem;
begin
  Result := TEVEContractCorporationItem(inherited Items[Index]);
end;

{ TEVEContractCorporationList }

function TEVEContractCorporationList.GetItems(Index: integer): TEVEContractCorporation;
begin
  Result := TEVEContractCorporation(inherited Items[Index]);
end;

{ TEVEContractCharacterItemList }

function TEVEContractCharacterItemList.GetItems(Index: integer): TEVEContractCharacterItem;
begin
  Result := TEVEContractCharacterItem(inherited Items[Index]);
end;

{ TEVEContractCharacterBidList }

function TEVEContractCharacterBidList.GetItems(Index: integer): TEVEContractCharacterBid;
begin
  Result := TEVEContractCharacterBid(inherited Items[Index]);
end;

{ TEVEContractItemList }

function TEVEContractItemList.GetItems(Index: integer): TEVEContractItem;
begin
  Result := TEVEContractItem(inherited Items[Index]);
end;

{ TEVEContractBidList }

function TEVEContractBidList.GetItems(Index: integer): TEVEContractBid;
begin
  Result := TEVEContractBid(inherited Items[Index]);
end;

{ TEVEContractList }

function TEVEContractList.GetItems(Index: integer): TEVEContract;
begin
  Result := TEVEContract(inherited Items[Index]);
end;

{ TEVEContractCharacterList }

function TEVEContractCharacterList.GetItems(Index: integer): TEVEContractCharacter;
begin
  Result := TEVEContractCharacter(inherited Items[Index]);
end;

{ TESIContracts }

function TESIContracts.GetContractsCharacter(const AAccessToken: string; ACharacterId: integer; APage: integer): string;
begin

end;

function TESIContracts.GetContractCharactersBids(const AAccessToken: string; ACharacterId, AContractID: integer): string;
begin

end;

function TESIContracts.GetContractCharactersItems(const AAccessToken: string; ACharacterId, AContractID, APage: integer): string;
begin

end;

function TESIContracts.GetPublicContracts(ARegionID, APage: integer): TEVEContractList;
const
  URL = 'https://esi.evetech.net/latest/contracts/public/%s/?datasource=%s&page=%d';
begin
  Result := TEVEContractList.Create(TEVEContract);
  DeStreamerArray(Get(Format(URL, [ARegionID.ToString, DataSource, APage])), TCollection(Result));
end;

function TESIContracts.GetPublicBids(AContractID: integer): string;
begin

end;

function TESIContracts.GetPublicItems(AContractID, APage: integer): string;
begin

end;

function TESIContracts.GetContractsCorporation(const AAccessToken: string; ACharacterId: integer; APage: integer): string;
begin

end;

function TESIContracts.GetContractCorporationBids(const AAccessToken: string; ACorporationId, AContractID: integer): string;
begin

end;

function TESIContracts.GetContractCorporationItems(const AAccessToken: string; ACorporationId, AContractID, APage: integer): string;
begin

end;

end.
