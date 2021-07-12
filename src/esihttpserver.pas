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
unit esihttpserver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver;

type
  { TEVEHTTPServer }
  TOnRequestNotify = procedure(Sender: TObject; AResult: string; ACode: string; AState: string) of object;
  TOnRequestErrorNotify = procedure(Sender: TObject; E: Exception) of object;

  TEVEHTTPServer = class(TThread)
  private
    FHTMLAnswer: string;
    FHTTPServer: TFPHttpServer;
    FOnRequestErrorNotify: TOnRequestErrorNotify;
    FOnRequestNotify: TOnRequestNotify;
    FStopAndFree: boolean;
    procedure AcceptIdleNotification(Sender: TObject);
    procedure Execute; override;
    procedure RequestErrorHandler(Sender: TObject; E: Exception);
    procedure RequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(APort: word; AHTMLAnswer: string);
    destructor Destroy; override;
    procedure StopAndFree;
    property OnRequest: TOnRequestNotify read FOnRequestNotify write FOnRequestNotify;
    property OnRequestError: TOnRequestErrorNotify read FOnRequestErrorNotify write FOnRequestErrorNotify;
  end;

implementation

{ TEVEHTTPServer }

procedure TEVEHTTPServer.Execute;
begin
  try
    FHTTPServer.Active := True;
  except
    on e: Exception do ;
  end;
  Terminate;
end;

procedure TEVEHTTPServer.AcceptIdleNotification(Sender: TObject);
begin
  if FStopAndFree then
    FHTTPServer.Active := False;
end;

procedure TEVEHTTPServer.RequestErrorHandler(Sender: TObject; E: Exception);
begin
  if Assigned(OnRequestError) then
    OnRequestError(Sender, E);
end;

procedure TEVEHTTPServer.RequestHandler(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  code, state: string;
begin
  if FStopAndFree then
    exit;
  AResponse.Contents.Add(FHTMLAnswer);
  code := ARequest.URL;
  code := copy(ARequest.URL, pos('=', ARequest.URL) + 1, Length(ARequest.URL));
  code := copy(code, 1, pos('&', code) - 1);
  state := copy(ARequest.URL, pos('&state=', ARequest.URL) + 1, Length(ARequest.URL));
  if Assigned(OnRequest) then
    OnRequest(Self, ARequest.URL, code, state);
  FHTTPServer.Active := False;
  Terminate;
end;

constructor TEVEHTTPServer.Create(APort: word; AHTMLAnswer: string);
begin
  inherited Create(True);
  FHTMLAnswer := AHTMLAnswer;
  FreeOnTerminate := True;
  FHTTPServer := TFPHttpServer.Create(nil);
  FHTTPServer.Port := APort;
  FHTTPServer.Threaded := True;
  FHTTPServer.OnAcceptIdle := @AcceptIdleNotification;
  FHTTPServer.OnRequest := @RequestHandler;
  FHTTPServer.OnRequestError := @RequestErrorHandler;
  FHTTPServer.AcceptIdleTimeout := 500;
  FStopAndFree := False;
end;

destructor TEVEHTTPServer.Destroy;
begin
  FreeAndNil(FHTTPServer);
  inherited Destroy;
end;

procedure TEVEHTTPServer.StopAndFree;
begin
  FStopAndFree := True;
end;

end.
