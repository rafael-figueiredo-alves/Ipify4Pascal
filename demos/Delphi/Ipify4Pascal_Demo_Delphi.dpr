program Ipify4Pascal_Demo_Delphi;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  LibIpify4Pascal in '..\..\src\LibIpify4Pascal.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
