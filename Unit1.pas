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
  FPascalLexer:=TDefaultLexer.Create(PascalLexerDefinition);  // Création du lexer pour les éléments syntaxiques du Pascal
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPascalLexer.Destroy;  // Libération de la mémoire occupée par la table du lexer
end;

procedure TForm1.SyntaxHighlighter1TokenizeLineClass(Sender: TObject;  // Evénement appelé pour déterminer la classe d'une ligne.
  const LastLineClass: Byte; var NewLineClass: Byte;                   // Par exemple, LastLineClass vaut 2 si la ligne précédante contenait une
  const TextBuffer: PAnsiChar; const TextLength: Integer);             // directive de précompilation non fermée. La valeur stockée dans
var                                                                    // NewLineClass servira pour la ligne d'après. Par convention, la première
  i:ICharBuffer;                                                       // ligne est de classe zéro.
  a:Integer;
begin
  NewLineClass:=LastLineClass;
  if TextLength=0 then
    Exit;
  i:=TDefaultCharBuffer.Create(TextBuffer+#13);
  FPascalLexer.Init(i);      // Initialisation du Lexer
  repeat                                      // Tant qu'on n'est pas à la fin de la ligne
    case NewLineClass of                      // en fonction de la classe de la ligne d'avant
      0:case FPascalLexer.GetNewLexem of      // si on est dans le langage Pascal "normal" on extrait un nouveau lexème
        3:NewLineClass:=1;                    // ...si c'est le début d'un bloc assembleur alors     passage à la classe 1
        11:NewLineClass:=2;                   // ...        ---       d'une directive de compilation         ---         2
        12:NewLineClass:=3;                   // ...        ---       d'un commentaire {}                    ---         3
        13:NewLineClass:=4;                   // ...        ---       d'un commentaire (* *)                 ---         4
      end;
      1:begin                                                       // Si la ligne d'avant contenait un bloc asm non fermé
        a:=PosEx('end',TextBuffer,FPascalLexer.GetLastLexerPos);    // on recherche le mot-clé "end" (A AMELIORER!)
        if a=0 then                                                 // S'il n'y est pas, pas la peine d'aller plus loin
          Break;
        NewLineClass:=0;                                            // Sinon, passage à la classe 0 (Pascal standard)
        FPascalLexer.SetLastLexerPos(a+3);
      end;
      2,3:begin                                                     // Si la ligne d'avant contenait une directive de compilation ou un commentaire {} non fermé
        a:=PosEx('}',TextBuffer,FPascalLexer.GetLastLexerPos);      // on recherche la fin de la directive ou du commentaire : "}"
        if a=0 then                                                 // Idem
          Break;
        NewLineClass:=0;
        FPascalLexer.SetLastLexerPos(a+1);
      end;
      4:begin                                                       // Si la ligne d'avant contenait un commentaire (* *) non fermé
        a:=PosEx('*)',TextBuffer,FPascalLexer.GetLastLexerPos);     // on recherche la fin du commentaire : "*)"
        if a=0 then                                                 // Idem
          Break;
        NewLineClass:=0;
        FPascalLexer.SetLastLexerPos(a+2);
      end;
    end;
  until FPascalLexer.GetLexerPos>Cardinal(TextLength);
  i:=nil;                                                           // Libération de la mémoire via _Release implicite
end;

procedure TForm1.SyntaxHighlighter1TokenizeLine(Sender: TObject;    // Evénement appelé pour déterminer la classe des caractères d'une ligne
  const LineClass: Byte; const TextBuffer: PAnsiChar;               // Line Class contient la classe de la ligne précédante déterminée par
  const TextLength: Integer; CharClass: PCharClassArray);           // l'événement ci-dessus
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
  repeat                                      // Tant qu'on n'est pas à la fin de la ligne...
    case l of                                 // En fonction de la classe courante...
      0:begin                                 // ...si on est dans du code Pascal, alors on extrait un lexème
        c:=FPascalLexer.GetNewLexem+1;
        case c of                             // En fonction de la valeur du lexème...
          4:l:=1;                             // si c'est de l'assembleur passage à la classe assembleur
          12:begin
            l:=2;                             // si c'est le début d'une directive de compilation changement de classe
            c:=10;                            // et changement du lexème pour avoir la bonne couleur
          end;
          13:begin
            l:=3;                             // si c'est le début d'un commentaire { } alors changement de classe
            c:=11;                            // et changement du lexème pour avoir la bonne couleur
          end;
          14:begin
            l:=4;                             // si c'est le début d'un commentaire (* *) alors changement de classe
            c:=11;                            // et changement du lexème pour avoir la bonne couleur
          end;
          17:c:=0;                            // Lexème non reconnu, on le traite comme du texte normal
        end;
        a:=FPascalLexer.GetLexerPos-FPascalLexer.GetLastLexerPos;
        FillChar(CharClass[FPascalLexer.GetLastLexerPos-1],Min(a,TextLength-Integer(FPascalLexer.GetLastLexerPos)+1),c); // La classe des caractères
      end;                                                                                                               // formant le lexème est définie
      1:begin                                                     // Si on est dans un bloc asm
        a:=PosEx('end',TextBuffer,FPascalLexer.GetLastLexerPos);  // on en cherche la fin
        if a=0 then
          a:=TextLength;                                          // en on remplit la ligne avec la classe de caractères correspondante
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
  i:=nil;                  // Libération de la mémoire
end;

end.
