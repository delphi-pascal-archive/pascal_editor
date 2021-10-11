program Project1;

{%File 'Pascal.lexer'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Pascal in 'Pascal.pas' {*.ppas};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
