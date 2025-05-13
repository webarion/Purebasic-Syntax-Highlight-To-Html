
; Purebasic Syntax Highlight To Html
; Author ..: Webarion
; Version .: 1.2 (13.05.2025)
; License .: Free

; Changes ...: * Fixed error in the comment regular
;            : * Added an example of transformation from the file template
;            : * Added the topics of the colors from IDE and additional topics

; Изменения .: * Исправлена ошибка в регулярке комментария
;            : * Добавлен пример преобразования из файла шаблона
;            : * Добавлены темы расцветок из IDE и дополнительные темы

EnableExplicit

;- Structure

Structure Symbol
  c.c[0]
EndStructure

;- Variables

Global NewMap SHL_Template.s()

Global NewList SHL_Index$()

Global Pattern_REGEX_SHL$ = "(?mi)"
Global REGEX_SHL


Procedure _Get_Count_Groups_Pattern_SHL( Pattern$ )
  Protected Counter = -1
  Protected REGEX_SHL_Groups = CreateRegularExpression( #PB_Any, Pattern$ )
  If IsRegularExpression( REGEX_SHL_Groups )
    Counter = CountRegularExpressionGroups( REGEX_SHL_Groups )
    FreeRegularExpression( REGEX_SHL_Groups )
    ProcedureReturn Counter
  EndIf
  ProcedureReturn Counter
EndProcedure

Procedure _Add_Pattern_REGEX_SHL( Name$, Pattern$ )
  Static Sep$ = "", i
  Protected CountGroups = _Get_Count_Groups_Pattern_SHL( Pattern$ )
  If CountGroups > 0
    For i = 1 To CountGroups
      AddElement( SHL_Index$() )
      SHL_Index$() = Name$ 
    Next
    Pattern_REGEX_SHL$ + Sep$ + Pattern$
    Sep$ = "|" 
  EndIf
EndProcedure

Procedure.a _Init_REGEX_SHL()
  REGEX_SHL = CreateRegularExpression( #PB_Any, Pattern_REGEX_SHL$  )
  ProcedureReturn REGEX_SHL
EndProcedure

; PureBasic IDE standard color regular expressions
; Регулярки стандартной расцветки IDE PureBasic
_Add_Pattern_REGEX_SHL( "stringcolor", "(~"+#DQUOTE$+"(?:\\.|[^\\])*?(?:"+#DQUOTE$+"|$)|"+#DQUOTE$+"[^"+#DQUOTE$+"]*?(?:"+#DQUOTE$+"|$))" )
_Add_Pattern_REGEX_SHL( "customkeywordcolor", "\b(Procedure|EndProcedure)\b" )
_Add_Pattern_REGEX_SHL( "basickeywordcolor", "\b(And|Array|As|Break|CallDebugger|Case|CompilerCase|CompilerDefault|CompilerElse|CompilerElseIf|CompilerEndIf|CompilerEndSelect|CompilerError|CompilerIf|CompilerSelect|CompilerWarning|Continue|Data|DataSection|EndDataSection|Debug|DebugLevel|Declare|DeclareC|DeclareCDLL|DeclareDLL|DeclareModule|EndDeclareModule|Default|Define|Dim|DisableASM|DisableDebugger|DisableExplicit|Else|ElseIf|EnableASM|EnableDebugger|EnableExplicit|End|Enumeration|EnumerationBinary|EndEnumeration|FakeReturn|For|ForEach|ForEver|Global|Gosub|Goto|If|EndIf|Import|EndImport|ImportC|IncludeBinary|IncludeFile|IncludePath|Interface|EndInterface|List|Macro|EndMacro|MacroExpandedCount|Map|Module|EndModule|NewList|NewMap|Next|Not|Or|Procedure|EndProcedure|ProcedureC|ProcedureCDLL|ProcedureDLL|ProcedureReturn|Protected|Prototype|PrototypeC|Read|ReDim|Repeat|Restore|Return|Runtime|Select|EndSelect|Shared|Static|Step|Structure|EndStructure|StructureUnion|EndStructureUnion|Swap|Threaded|To|UndefineMacro|Until|UseModule|UnuseModule|Wend|While|With|EndWith|XIncludeFile|XOr|Align|Extends)\b" )
_Add_Pattern_REGEX_SHL( "commentcolor", "(;.*(?:\r\n|\r|\n))" )
_Add_Pattern_REGEX_SHL( "constantcolor", "(\#[a-z_]\w*\$?)" )
_Add_Pattern_REGEX_SHL( "purekeywordcolor", "(\w+\$?(?=(?:\s*\.\s*\w+\s*|\s*)\())" )
_Add_Pattern_REGEX_SHL( "asmkeywordcolor", "^\s*(!.*?)(?=;|$)" )
_Add_Pattern_REGEX_SHL( "pointercolor", "((?:\@\*?|\*|\?)[a-z_]\w+(?:\$|\(\))?)" )
_Add_Pattern_REGEX_SHL( "numbercolor", "((?:\b\d+\.?\d*(?:e[+\-]?\d+)?|\$[\dabcdef]+|\%[01]+))" )
_Add_Pattern_REGEX_SHL( "structurecolor", "([a-z_]\w*(?=\s*\.[^abcdfilqsuw]))|(?![\r\n])(?<=\.)\s*([^abcdfilqsuw][a-z_]\w*)|([a-z_]\w*(?=\s*\\)(?!\r\n))|(?<=\\)\s*([a-z_]\w*)" )
_Add_Pattern_REGEX_SHL( "modulecolor", "(\w+(?=\s*::))" )
_Add_Pattern_REGEX_SHL( "labelcolor", "(^\s*\w+\$?\s*:(?!:))" )
_Add_Pattern_REGEX_SHL( "operatorcolor", "((?:[+*/\-|!%=~]|::|&gt;|&lt;|&amp;)+)" )
_Add_Pattern_REGEX_SHL( "separatorcolor", "([()\[\]\\,.:])" )

_Init_REGEX_SHL()

; Conversion Procedure
; Процедура преобразования
Procedure.s PB_Syntax_Highlighting( Code$ )  
  Protected Result$ = "", i
  Code$ = ReplaceString( Code$, "&", "&amp;" )  
  Code$ = ReplaceString( Code$, "<", "&lt;" )
  Code$ = ReplaceString( Code$, ">", "&gt;" )
  If IsRegularExpression( REGEX_SHL ) And MatchRegularExpression( REGEX_SHL, Code$ ) And ExamineRegularExpression( REGEX_SHL, Code$ )
    Protected PrevPos = 1
    While NextRegularExpressionMatch( REGEX_SHL )
      Protected CountGroups = CountRegularExpressionGroups( REGEX_SHL )
      If CountGroups
        For i = 1 To CountGroups
          Protected Replacement_Code$ = Trim( RegularExpressionGroup( REGEX_SHL, i ) )
          If Replacement_Code$
            Protected Position = RegularExpressionMatchPosition( REGEX_SHL ) + RegularExpressionGroupPosition( REGEX_SHL, i ) - 1
            Protected FirstCode$ = Mid( Code$, PrevPos, Position - PrevPos ) 
            SelectElement( SHL_Index$(), i-1 )
            Protected Template$ = SHL_Template( SHL_Index$() )
            If Template$
              Template$ = ReplaceString( Template$, "%1", Replacement_Code$)
              Result$ + FirstCode$ + Template$
              PrevPos = Position + Len(Replacement_Code$)
            EndIf
          EndIf  
        Next
      EndIf
    Wend
  EndIf
  ProcedureReturn Result$
EndProcedure

; template loading procedure
; процедура загрузки шаблона
Procedure.s Load_INI_File_SHL( PathFile$, Section$ )
  
  OpenPreferences( PathFile$ )
  
  PreferenceGroup(Section$)

  SHL_Template("normaltextcolor") = ReadPreferenceString("NormalTextColor", "#FFFFFF" )
  SHL_Template("backgroundcolor") = ReadPreferenceString("BackgroundColor", "#002240" )
  
  SHL_Template("labelcolor")         = ReadPreferenceString( "LabelColor",         "<font color='#D33682'>%1</font>" )
  SHL_Template("numbercolor")        = ReadPreferenceString( "NumberColor",        "<font color='#FF628C'>%1</font>" )
  SHL_Template("stringcolor")        = ReadPreferenceString( "StringColor",        "<font color='#3AD900'>%1</font>" )
  SHL_Template("modulecolor")        = ReadPreferenceString( "ModuleColor",        "<font color='#CB4B16'>%1</font>" )
  SHL_Template("pointercolor")       = ReadPreferenceString( "PointerColor",       "<font color='#5DAEF2'>%1</font>" )
  SHL_Template("commentcolor")       = ReadPreferenceString( "CommentColor",       "<font color='#0088FF'>%1</font>" )
  SHL_Template("constantcolor")      = ReadPreferenceString( "ConstantColor",      "<font color='#FF628C'>%1</font>" )
  SHL_Template("operatorcolor")      = ReadPreferenceString( "OperatorColor",      "<font color='#FF9D00'>%1</font>" )
  SHL_Template("separatorcolor")     = ReadPreferenceString( "SeparatorColor",     "<font color='#FFD715'>%1</font>" )
  SHL_Template("structurecolor")     = ReadPreferenceString( "StructureColor",     "<font color='#60B66D'>%1</font>" )
  SHL_Template("asmkeywordcolor")    = ReadPreferenceString( "ASMKeywordColor",    "<font color='#6C71C4'>%1</font>" ) 
  SHL_Template("purekeywordcolor")   = ReadPreferenceString( "PureKeywordColor",   "<font color='#FFB454'>%1</font>" )
  SHL_Template("basickeywordcolor")  = ReadPreferenceString( "BasicKeywordColor",  "<font color='#FFB054'>%1</font>" )
  SHL_Template("customkeywordcolor") = ReadPreferenceString( "CustomKeywordColor", "<font color='#FB86CD'>%1</font>" )
  
  ClosePreferences()
  
EndProcedure 


Procedure.s Load_File_To_Var(FilePath$)
  Protected FileText$, File = ReadFile(#PB_Any, FilePath$)  
  If File  
    Protected Format = ReadStringFormat(File)
    FileText$ = ReadString( File, Format | #PB_File_IgnoreEOL )
    CloseFile(File)
  Else
    Debug "Error - Couldn't open file: " + FilePath$
  EndIf
  ProcedureReturn FileText$
EndProcedure


; Converts a variable with a template into a ready HTML code. Преобразует переменную с шаблоном в готовый html код
Procedure.s Convert_Code(TmplHTML$, PBCode$)
  Protected Position = 0
  Protected *Start.Symbol = @TmplHTML$
  Protected *Next.Symbol  = *Start
  Protected NewList Key$()
  Repeat
    Position + 1
    Position = FindString( TmplHTML$, "{SHL_", Position, #PB_String_NoCase )
    If Position
      *Next = *Start + ((Position+4)*2)
      Protected *StartKey.Symbol = *Next
      While (*Next\c>='a' And *Next\c<='z') Or (*Next\c>='A' And *Next\c<='Z')
        *Next + 2
      Wend
      If *Next\c = '}'
        Protected Key$ = LCase( PeekS( *StartKey, ( *Next - *StartKey ) / 2 ) )
        If Key$ <> "code"
          AddElement( Key$() ) : Key$() = Key$
        EndIf
      EndIf
    EndIf
  Until Not Position
  
  ForEach Key$()
    TmplHTML$ = ReplaceString( TmplHTML$, "{SHL_" + Key$() + "}", SHL_Template( Key$() ), #PB_String_NoCase )
  Next
  TmplHTML$ = ReplaceString( TmplHTML$, "{SHL_CODE}", PB_Syntax_Highlighting( PBCode$ ), #PB_String_NoCase )
  
  ProcedureReturn TmplHTML$
EndProcedure


; Converts the template file into ready-made HTML code. Преобразует файл шаблона в готовый html код
Procedure.s Convert_Template_File(FilePath$, PBCode$)
  Protected TmplHTML$ = Load_File_To_Var(FilePath$)
  ProcedureReturn Convert_Code(TmplHTML$, PBCode$)
EndProcedure



;- EXAMPLE. ПРИМЕР

CompilerIf #PB_Compiler_IsMainFile
  If OpenWindow( 0, 0, 0, 800, 500, "", #PB_Window_SystemMenu | #PB_Window_ScreenCentered )
    
    WebGadget( 0, 10, 10, 780, 480, "" )
    
    Load_INI_File_SHL( "", "" ) ; Load default style. Грузим стиль по умолчанию
    
    ; You can upload the theme from the file. Можно загружать тему из файла
;     Load_INI_File_SHL( "Themes" + #PS$ + "Theme_Cobalt.ini", "" ) 
    
    Define HTML$ = ""
    
    ; Try 0 or 1 to see different methods of transformation
    ; Попробуйте 0 или 1, чтобы увидеть разные способы преобразования
    #EXAMPLE_TRANSFORM = 1
    
    If Not #EXAMPLE_TRANSFORM
      ; Direct transformation. Непосредственное преобразование
      Define TmplHTML$ = "<head><meta charset='utf-8'></head>" +
                         "<body>" +
                         "  <pre style='color:{SHL_NormalTextColor}; background:{SHL_BackgroundColor};font-size:14;display:table;padding:10px;'>" +
                         "    <code>" +
                         "      {SHL_CODE}" +
                         "    </code>" +
                         "  </pre>" +
                         "</body>"
      HTML$ = Convert_Code( TmplHTML$, Load_File_To_Var( #PB_Compiler_File )  )
    Else
      ; Transformation from the template file. Преобразование из файла шаблона
      HTML$ = Convert_Template_File( "Template_HTML.html", Load_File_To_Var( #PB_Compiler_File ) )
    EndIf  
    
    
    SetGadgetItemText( 0, #PB_Web_HtmlCode, HTML$ )
    
    Repeat
    Until WaitWindowEvent() = #PB_Event_CloseWindow
  EndIf
  
CompilerEndIf
