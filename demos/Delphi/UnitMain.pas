unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    BtnGetIPv4: TButton;
    BtnGetIPv6: TButton;
    BtnGeIpifyVersion: TButton;
    lbIPv4: TLabel;
    lbIPv6: TLabel;
    lbIpifyVersion: TLabel;
    procedure BtnGetIPv4Click(Sender: TObject);
    procedure BtnGeIpifyVersionClick(Sender: TObject);
    procedure BtnGetIPv6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses LibIpify4Pascal;

procedure TForm1.BtnGeIpifyVersionClick(Sender: TObject);
begin
   lbIpifyVersion.Text := Ipify4Pascal.Version;
end;

procedure TForm1.BtnGetIPv4Click(Sender: TObject);
begin
  lbIPv4.Text := Ipify4Pascal.GetIPv4;
end;

procedure TForm1.BtnGetIPv6Click(Sender: TObject);
begin
  lbIPv6.Text := Ipify4Pascal.GetIPv6;
end;

end.
