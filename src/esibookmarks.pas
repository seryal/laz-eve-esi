unit esibookmarks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase;

type

  { TEVEBookmarkCoordinates }

  TEVEBookmarkCoordinates = class(TCollectionItem)
  private
    Fx: double;
    Fy: double;
    Fz: double;
  published
    property x: double read Fx write Fx;
    property y: double read Fy write Fy;
    property z: double read Fz write Fz;
  end;

  { TEVEBookmarkItem }

  TEVEBookmarkItem = class(TCollectionItem)
  private
    Fitem_id: uint64;
    Ftype_id: integer;
  published
    property item_id: uint64 read Fitem_id write Fitem_id;
    property type_id: integer read Ftype_id write Ftype_id;
  end;

  { TEVEBookmark }

  TEVEBookmark = class(TCollectionItem)
  private
    Fbookmark_id: integer;
    Fcoordinates: TEVEBookmarkCoordinates;
    Fcreated: string;
    Fcreator_id: integer;
    Ffolder_id: integer;
    Fitem: TEVEBookmarkItem;
    Flabel: string;
    Flocation_id: integer;
    Fnotes: string;
  published
    property bookmark_id: integer read Fbookmark_id write Fbookmark_id;
    property coordinates: TEVEBookmarkCoordinates read Fcoordinates write Fcoordinates;
    property created: string read Fcreated write Fcreated;
    property creator_id: integer read Fcreator_id write Fcreator_id;
    property folder_id: integer read Ffolder_id write Ffolder_id;
    property item: TEVEBookmarkItem read Fitem write Fitem;
    property &label: string read Flabel write Flabel;
    property location_id: integer read Flocation_id write Flocation_id;
    property notes: string read Fnotes write Fnotes;
  end;

  { TEVEBookmarkList }

  TEVEBookmarkList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEBookmark;
  public
    property Items[Index: integer]: TEVEBookmark read GetItems;
  end;

  { TEVEBookmarksFolder }

  TEVEBookmarksFolder = class(TCollectionItem)
  private
    Ffolder_id: integer;
    Fname: string;
  published
    property folder_id: integer read Ffolder_id write Ffolder_id;
    property &name: string read Fname write Fname;
  end;

  { TEVEBookmarksFolderList }

  TEVEBookmarksFolderList = class(TCollection)
  private
    function GetItems(Index: integer): TEVEBookmarksFolder;
  public
    property Items[Index: integer]: TEVEBookmarksFolder read GetItems;
  end;

  { TESIBookmarks }

  TESIBookmarks = class(TESIBase)
  public
    function GetCharacterBookmarks(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarkList;
    function GetCharacterBookmarksFolders(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarksFolderList;
    function GetCorporationBookmarks(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarkList;
    function GetCorporationBookmarksFolders(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarksFolderList;
  end;

implementation

{ TEVEBookmarksFolderList }

function TEVEBookmarksFolderList.GetItems(Index: integer): TEVEBookmarksFolder;
begin
  Result := TEVEBookmarksFolder(inherited Items[Index]);
end;

{ TEVEBookmarkList }

function TEVEBookmarkList.GetItems(Index: integer): TEVEBookmark;
begin
  Result := TEVEBookmark(inherited Items[Index]);
end;

{ TESIBookmarks }

function TESIBookmarks.GetCharacterBookmarks(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarkList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/bookmarks/?datasource=%s&page=%d';
begin
  Result := TEVEBookmarkList.Create(TEVEBookmark);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

function TESIBookmarks.GetCharacterBookmarksFolders(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarksFolderList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/bookmarks/folders/?datasource=%s&page=%d';
begin
  Result := TEVEBookmarksFolderList.Create(TEVEBookmarksFolder);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

function TESIBookmarks.GetCorporationBookmarks(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarkList;
const
  URL = 'https://esi.evetech.net/latest/corporations/%s/bookmarks/?datasource=%s&page=%d';
begin
  Result := TEVEBookmarkList.Create(TEVEBookmark);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

function TESIBookmarks.GetCorporationBookmarksFolders(AAccessToken: string; ACharacterId, APage: integer): TEVEBookmarksFolderList;
const
  URL = 'https://esi.evetech.net/latest/corporations/%s/bookmarks/folders/?datasource=%s&page=%d';
begin
  Result := TEVEBookmarksFolderList.Create(TEVEBookmarksFolder);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource, APage])), TCollection(Result));
end;

end.
