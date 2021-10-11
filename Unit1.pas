unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SyntaxHighlighter, VirtualScrollingWinControl,
  SyntaxHighlightMemo, Math, Pascal, COW_RunTime, StrUtils, ComCtrls,
  TextEditorFooter;

type
  TForm1 = class(TForm)
    SyntaxHighlightMemo1: TSyntaxHighlightMemo;
    SyntaxHighlighter1: TSyntaxHighlighter;
    TextEditorFooter1: TTextEditorFooter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SyntaxHighlighter1TokenizeLineClass(Sender: TObject;
      const LastLineClass: Byte; var NewLineClass: Byte;
      const TextBuffer: PAnsiChar; const TextLength: Integer);
    procedure SyntaxHighlighter1TokenizeLine(Sender: TObject;
      const LineClass: Byte; const TextBuffer: PAnsiChar;
      const TextLength: Integer; CharClass: PCharClassArray);
  private
    FPascalLexer:TDefaultLexer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPascalLexer:=TDefaultLexer.Create(PascalLexerDefinition);  // Cr�ation du lexer pour les �l�ments syntaxiques du Pascal
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPascalLexer.Destroy;  // Lib�ration de la m�moire occup�e par la table du lexer
end;

procedure TForm1.SyntaxHighlighter1TokenizeLineClass(Sender: TObject;  // Ev�nement appel� pour d�terminer la classe d'une ligne.
  const LastLineClass: Byte; var NewLineClass: Byte;                   // Par exemple, LastLineClass vaut 2 si la ligne pr�c�dante contenait une
  const TextBuffer: PAnsiChar; const TextLength: Integer);             // directive de pr�compilation non ferm�e. La valeur stock�e dans
var                                                                    // NewLineClass servira pour la ligne d'apr�s. Par convention, la premi�re
  i:ICharBuffer;                                                       // ligne est de classe z�ro.
  a:Integer;
begin
  NewLineClass:=LastLineClass;
  if TextLength=0 then
    Exit;
  i:=TDefaultCharBuffer.Create(TextBuffer+#13);
  FPascalLexer.Init(i);      // Initialisation du Lexer
  repeat                                      // Tant qu'on n'est pas � la fin de la ligne
    case NewLineClass of                      // en fonction de la classe de la ligne d'avant
      0:case FPascalLexer.GetNewLexem of      // si on est dans le langage Pascal "normal" on extrait un nouveau lex�me
        3:NewLineClass:=1;                    // ...si c'est le d�but d'un bloc assembleur alors     passage � la classe 1
        11:NewLineClass:=2;                   // ...        ---       d'une directive de compilation         ---         2
        12:NewLineClass:=3;                   // ...        ---       d'un commentaire {}                    ---         3
        13:NewLineClass:=4;                   // ...        ---       d'un commentaire (* *)                 ---         4
      end;
      1:begin                                                       // Si la ligne d'avant contenait un bloc asm non ferm�
        a:=PosEx('end',TextBuffer,FPascalLexer.GetLastLexerPos);    // on recherche le mot-cl� "end" (A AMELIORER!)
        if a=0 then                                                 // S'il n'y est pas, pas la peine d'aller plus loin
          Break;
        NewLineClass:=0;                                            // Sinon, passage � la classe 0 (Pascal standard)
        FPascalLexer.SetLastLexerPos(a+3);
      end;
      2,3:begin                                                     // Si la ligne d'avant contenait une directive de compilation ou un commentaire {} non ferm�
        a:=PosEx('}',TextBuffer,FPascalLexer.GetLastLexerPos);      // on recherche la fin de la directive ou du commentaire : "}"
        if a=0 then                                                 // Idem
          Break;
        NewLineClass:=0;
        FPascalLexer.SetLastLexerPos(a+1);
      end;
      4:begin                                                       // Si la ligne d'avant contenait un commentaire (* *) non ferm�
        a:=PosEx('*)',TextBuffer,FPascalLexer.GetLastLexerPos);     // on recherche la fin du commentaire : "*)"
        if a=0 then                                                 // Idem
          Break;
        NewLineClass:=0;
        FPascalLexer.SetLastLexerPos(a+2);
      end;
    end;
  until FPascalLexer.GetLexerPos>Cardinal(TextLength);
  i:=nil;                                                           // Lib�ration de la m�moire via _Release implicite
end;

procedure TForm1.SyntaxHighlighter1TokenizeLine(Sender: TObject;    // Ev�nement appel� pour d�terminer la classe des caract�res d'une ligne
  const LineClass: Byte; const TextBuffer: PAnsiChar;               // Line Class contient la classe de la ligne pr�c�dante d�termin�e par
  const TextLength: Integer; CharClass: PCharClassArray);           // l'�v�nement ci-dessus
var
  i:ICharBuffer;
  c,l:Byte;
  a:Integer;
begin
  if TextLength=0 then
    Exit;
  i:=TDefaultCharBuffer.Create(TextBuffer+#13);
  FPascalLexer.Init(i);                                             // Initialisation du Lexer
  l:=LineClass;
  repeat                                      // Tant qu'on n'est pas � la fin de la ligne...
    case l of                                 // En fonction de la classe courante...
      0:begin                                 // ...si on est dans du code Pascal, alors on extrait un lex�me
        c:=FPascalLexer.GetNewLexem+1;
        case c of                             // En fonction de la valeur du lex�me...
          4:l:=1;                             // si c'est de l'assembleur passage � la classe assembleur
          12:begin
            l:=2;                             // si c'est le d�but d'une directive de compilation changement de classe
            c:=10;                            // et changement du lex�me pour avoir la bonne couleur
          end;
          13:begin
            l:=3;                             // si c'est le d�but d'un commentaire { } alors changement de classe
            c:=11;                            // et changement du lex�me pour avoir la bonne couleur
          end;
          14:begin
            l:=4;                             // si c'est le d�but d'un commentaire (* *) alors changement de classe
            c:=11;                            // et changement du lex�me pour avoir la bonne couleur
          end;
          17:c:=0;                            // Lex�me non reconnu, on le traite comme du texte normal
        end;
        a:=FPascalLexer.GetLexerPos-FPascalLexer.GetLastLexerPos;
        FillChar(CharClass[FPascalLexer.GetLastLexerPos-1],Min(a,TextLength-Integer(FPascalLexer.GetLastLexerPos)+1),c); // La classe des caract�res
      end;                                                                                                               // formant le lex�me est d�finie
      1:begin                                                     // Si on est dans un bloc asm
        a:=PosEx('end',TextBuffer,FPascalLexer.GetLastLexerPos);  // on en cherche la fin
        if a=0 then
          a:=TextLength;                                          // en on remplit la ligne avec la classe de caract�res correspondante
        FillChar(CharClass[FPascalLexer.GetLastLexerPos-1],Min(a-Integer(FPascalLexer.GetLastLexerPos)+4,TextLength-Integer(FPascalLexer.GetLastLexerPos)+1),4);
        FPascalLexer.SetLastLexerPos(a+3);
        l:=0;
      end;
      2:begin                                                     // Idem avec les directives de compilation
        a:=PosEx('}',TextBuffer,FPascalLexer.GetLastLexerPos);
        if a=0 then
          a:=TextLength;
        FillChar(CharClass[FPascalLexer.GetLastLexerPos-1],Min(a-Integer(FPascalLexer.GetLastLexerPos)+2,TextLength-Integer(FPascalLexer.GetLastLexerPos)+1),10);
        FPascalLexer.SetLastLexerPos(a+1);
        l:=0;
      end;
      3:begin                                                     // Idem avec les commentaires { }
        a:=PosEx('}',TextBuffer,FPascalLexer.GetLastLexerPos);
        if a=0 then
          a:=TextLength;
        FillChar(CharClass[FPascalLexer.GetLastLexerPos-1],Min(a-Integer(FPascalLexer.GetLastLexerPos)+2,TextLength-Integer(FPascalLexer.GetLastLexerPos)+1),11);
        FPascalLexer.SetLastLexerPos(a+1);
        l:=0;
      end;
      4:begin                                                     // Idem avec les commentaires (* *)
        a:=PosEx('*)',TextBuffer,FPascalLexer.GetLastLexerPos);
        if a=0 then
          a:=TextLength;
        FillChar(CharClass[FPascalLexer.GetLastLexerPos-1],Min(a-Integer(FPascalLexer.GetLastLexerPos)+3,TextLength-Integer(FPascalLexer.GetLastLexerPos)+1),11);
        FPascalLexer.SetLastLexerPos(a+2);
        l:=0;
      end;
    end;
  until FPascalLexer.GetLexerPos>Cardinal(TextLength);
  i:=nil;                  // Lib�ration de la m�moire
end;

end.
