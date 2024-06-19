unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnIPv4: TButton;
    BtnIPv6: TButton;
    BtnIpifyVersion: TButton;
    lbIpv4: TLabel;
    lbIpv6: TLabel;
    lbIpifyVersion: TLabel;
    procedure BtnIpifyVersionClick(Sender: TObject);
    procedure BtnIPv4Click(Sender: TObject);
    procedure BtnIPv6Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

Uses LibIpify4Pascal;

{ TForm1 }

procedure TForm1.BtnIPv4Click(Sender: TObject);
var
  HttpClient : TFPHTTPClient;
  s: string;
begin
  HttpClient := TFPHTTPClient.Create(nil);
  try
    S := HttpClient.Get('https://api.ipify.org');
  finally
    HttpClient.Free;
  end;


  lbIpv4.Caption := S;//Ipify4Pascal.GetIPv4;
end;

procedure TForm1.BtnIpifyVersionClick(Sender: TObject);
begin
  lbIpifyVersion.Caption := Ipify4Pascal.Version;
end;

procedure TForm1.BtnIPv6Click(Sender: TObject);
begin
  lbIpv6.Caption := Ipify4Pascal.GetIPv6;
end;

end.

