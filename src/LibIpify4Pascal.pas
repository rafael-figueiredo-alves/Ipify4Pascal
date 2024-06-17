unit LibIpify4Pascal;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
     {$IFDEF FPC}
      idHTTP,
      IdSSLOpenSSL,
      IdSSLOpenSSLHeaders;
     {$else}
      Net.HttpClient;
     {$endif}

type
  //Enum to tell which kind of IP to get
  TIPKind = (IPv4, IPv6);

  //Interface of the library
  iIpify4Pascal = interface
    ['{A9FAD1F5-9060-4B47-93E8-0AF8F7D50365}']
    function GetIPv4: string;
    function GetIPv6: string;
    function Version: string;
  end;

  //Concrete class to implement the interface
  TIpify4Pascal = class(TInterfacedObject, iIpify4Pascal)
      //Version of the lib
      const _Version = '1.0.0';

      //IP4 URL
      const GetIPv4URL = 'https://api.ipify.org';

      //IP6 URL
      const GetIPv6URL = 'https://api6.ipify.org';
    private
      {$IFDEF FPC}
        HttpClient : TIdHTTP;
        FIdSSLIOHandlerSocketOpenSSL : TIdSSLIOHandlerSocketOpenSSL;
      {$else}
        HttpClient : THttpClient;
      {$endif}

      //Function to get the ip
      function GetIP(IPKind: TIPKind): string;
    public
      constructor Create;
      destructor Destroy; override;

      class function New: iIpify4Pascal;

      function GetIPv4: string;
      function GetIPv6: string;
      function Version: string;
  end;


  function Ipify4Pascal: iIpify4Pascal;

implementation

uses
  SysUtils, Classes, IdURI;

{ TIpify4Pascal }

/// <summary>
///   Starting point of the library
/// </summary>
/// <returns>
///   The Ipify4Pascal's interface
/// </returns>
function Ipify4Pascal: iIpify4Pascal;
begin
  Result := TIpify4Pascal.New;
end;

constructor TIpify4Pascal.Create;
begin
  {$IFDEF FPC}
   HttpClient := TIdHTTP.Create(nil);
   HttpClient.Request.Connection := 'Keep-Alive';
   HttpClient.Request.UserAgent := 'User-Agent:Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.96 Safari/537.36';
   HttpClient.HandleRedirects := true;
   FIdSSLIOHandlerSocketOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create;
   HttpClient.IOHandler := FIdSSLIOHandlerSocketOpenSSL;
   FIdSSLIOHandlerSocketOpenSSL.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
   HttpClient.HTTPOptions:= [hoKeepOrigProtocol];
  {$else}
   HttpClient := THTTPClient.Create;
  {$endif}
end;

destructor TIpify4Pascal.Destroy;
begin
  FreeAndNil(HttpClient);
  {$IFDEF FPC}
  FreeAndNil(FIdSSLIOHandlerSocketOpenSSL);
  {$endif}
  inherited;
end;

function TIpify4Pascal.GetIP(IPKind: TIPKind): string;
var
 FStreamResult : TStringStream;
 URL           : string;
begin
  case IPKind of
    IPv4: URL := GetIPv4URL;
    IPv6: URL := GetIPv6URL;
  end;
  try
    FStreamResult := TStringStream.Create;
    {$IFDEF FPC}
     HttpClient.Get(tidURI.URLEncode(GetIPv4URL), FStreamResult);
    {$else}
     HttpClient.Get(URL, FStreamResult);
    {$endif}
    Result := FStreamResult.DataString;
  finally
    {$IFDEF FPC}
    FreeAndNil(FStreamResult);
    {$endif}
  end;
end;

/// <summary>
///   Get the IPv4
/// </summary>
/// <returns>
///   string containing IPv4
/// </returns>
function TIpify4Pascal.GetIPv4: string;
begin
  Result := GetIP(IPv4);
end;

/// <summary>
///   Get the IPv6
/// </summary>
/// <returns>
///   string containing IPv6
/// </returns>
function TIpify4Pascal.GetIPv6: string;
begin
  Result := GetIP(IPv6);
end;

class function TIpify4Pascal.New: iIpify4Pascal;
begin
  Result := self.Create;
end;

/// <summary>
///   Get the Ipify4Pascal's version
/// </summary>
/// <returns>
///   string containing the Ipify4Pascal's version
/// </returns>
function TIpify4Pascal.Version: string;
begin
  Result := 'Ipify4Pascal version ' + _Version;
end;

end.
