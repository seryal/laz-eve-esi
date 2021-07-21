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
unit esicalendar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, esibase;

type

  { TEVECharacterCalendar }

  TEVECharacterCalendar = class(TCollectionItem)
  private
    Fevent_date: string;
    Fevent_id: integer;
    Fevent_response: string;
    Fimportance: integer;
    Ftitle: string;
  published
    property event_date: string read Fevent_date write Fevent_date;
    property event_id: integer read Fevent_id write Fevent_id;
    property event_response: string read Fevent_response write Fevent_response;
    property importance: integer read Fimportance write Fimportance;
    property title: string read Ftitle write Ftitle;
  end;

  { TEVECharacterCalendarList }

  TEVECharacterCalendarList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECharacterCalendar;
  public
    property Items[Index: integer]: TEVECharacterCalendar read GetItems;
  end;

  { TEVECalendarEvent }

  TEVECalendarEvent = class(TCollectionItem)
  private
    Fdate: string;
    Fduration: integer;
    Fevent_id: integer;
    Fimportance: integer;
    Fowner_id: integer;
    Fowner_name: string;
    Fowner_type: string;
    Fresponse: string;
    Ftext: string;
    Ftitle: string;
  published
    property date: string read Fdate write Fdate;
    property duration: integer read Fduration write Fduration;
    property event_id: integer read Fevent_id write Fevent_id;
    property importance: integer read Fimportance write Fimportance;
    property owner_id: integer read Fowner_id write Fowner_id;
    property owner_name: string read Fowner_name write Fowner_name;
    property owner_type: string read Fowner_type write Fowner_type;
    property response: string read Fresponse write Fresponse;
    property &text: string read Ftext write Ftext;
    property title: string read Ftitle write Ftitle;
  end;

  TEVEResponseEnum = (rspAccepted, rspDeclined, rspTentative);

  { TEVECalendarAttendee }

  TEVECalendarAttendee = class(TCollectionItem)
  private
    Fcharacter_id: integer;
    Fevent_response: string;
  published
    property character_id: integer read Fcharacter_id write Fcharacter_id;
    property event_response: string read Fevent_response write Fevent_response;
  end;

  { TEVECalendarAttendeeList }

  TEVECalendarAttendeeList = class(TCollection)
  private
    function GetItems(Index: integer): TEVECalendarAttendee;
  public
    property Items[Index: integer]: TEVECalendarAttendee read GetItems;
  end;

  { TESICalendar }

  TESICalendar = class(TESIBase)
  public
    function GetCalendar(AAccessToken: string; ACharacterId: integer): TEVECharacterCalendarList;
    function GetEvent(AAccessToken: string; ACharacterId, AEventId: integer): TEVECalendarEvent;
    function SetResponse(AAccessToken: string; ACharacterId, AEventId: integer; AResponse: TEVEResponseEnum): boolean;
    function GetAttendees(AAccessToken: string; ACharacterId, AEventId: integer): TEVECalendarAttendeeList;
  end;


implementation

{ TEVECalendarAttendeeList }

function TEVECalendarAttendeeList.GetItems(Index: integer): TEVECalendarAttendee;
begin
  Result := TEVECalendarAttendee(inherited Items[Index]);
end;

{ TESICalendar }

function TESICalendar.GetCalendar(AAccessToken: string; ACharacterId: integer): TEVECharacterCalendarList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/calendar/?datasource=%s';
begin
  Result := TEVECharacterCalendarList.Create(TEVECharacterCalendar);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, DataSource])), TCollection(Result));
end;

function TESICalendar.GetEvent(AAccessToken: string; ACharacterId, AEventId: integer): TEVECalendarEvent;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/calendar/%s/?datasource=%s';
begin
  Result := TEVECalendarEvent.Create(nil);
  DeStreamerObject(Get(AAccessToken, Format(URL, [ACharacterId.ToString, AEventId.ToString, DataSource])), TObject(Result));
end;

function TESICalendar.SetResponse(AAccessToken: string; ACharacterId, AEventId: integer; AResponse: TEVEResponseEnum): boolean;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/calendar/%s/?datasource=%s';
  RESPONSE = '{"response":"%s"}';
var
  res: string;
begin
  Result := False;
  case AResponse of
    rspAccepted: res := Format(RESPONSE, ['accepted']);
    rspDeclined: res := Format(RESPONSE, ['declined']);
    rspTentative: res := Format(RESPONSE, ['tentative']);
  end;
  res := Put(AAccessToken, Format(URL, [ACharacterId.ToString, AEventId.ToString, DataSource]), res);
  Result := True;
end;

function TESICalendar.GetAttendees(AAccessToken: string; ACharacterId, AEventId: integer): TEVECalendarAttendeeList;
const
  URL = 'https://esi.evetech.net/latest/characters/%s/calendar/%s/attendees/?datasource=%s';
begin
  Result := TEVECalendarAttendeeList.Create(TEVECalendarAttendee);
  DeStreamerArray(Get(AAccessToken, Format(URL, [ACharacterId.ToString, AEventId.ToString, DataSource])), TCollection(Result));
end;

{ TEVECharacterCalendarList }

function TEVECharacterCalendarList.GetItems(Index: integer): TEVECharacterCalendar;
begin
  Result := TEVECharacterCalendar(inherited Items[Index]);
end;

end.
