
; Purebasic Syntax Highlight To Html
; Author: Webarion

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
_Add_Pattern_REGEX_SHL( "StringColor", "(~"+#DQUOTE$+"(?:\\.|[^\\])*?(?:"+#DQUOTE$+"|$)|"+#DQUOTE$+"[^"+#DQUOTE$+"]*?(?:"+#DQUOTE$+"|$))" )
_Add_Pattern_REGEX_SHL( "CustomKeywordColor", "\b(Procedure|EndProcedure)\b" )
_Add_Pattern_REGEX_SHL( "BasicKeywordColor", "\b(And|Array|As|Break|CallDebugger|Case|CompilerCase|CompilerDefault|CompilerElse|CompilerElseIf|CompilerEndIf|CompilerEndSelect|CompilerError|CompilerIf|CompilerSelect|CompilerWarning|Continue|Data|DataSection|EndDataSection|Debug|DebugLevel|Declare|DeclareC|DeclareCDLL|DeclareDLL|DeclareModule|EndDeclareModule|Default|Define|Dim|DisableASM|DisableDebugger|DisableExplicit|Else|ElseIf|EnableASM|EnableDebugger|EnableExplicit|End|Enumeration|EnumerationBinary|EndEnumeration|FakeReturn|For|ForEach|ForEver|Global|Gosub|Goto|If|EndIf|Import|EndImport|ImportC|IncludeBinary|IncludeFile|IncludePath|Interface|EndInterface|List|Macro|EndMacro|MacroExpandedCount|Map|Module|EndModule|NewList|NewMap|Next|Not|Or|Procedure|EndProcedure|ProcedureC|ProcedureCDLL|ProcedureDLL|ProcedureReturn|Protected|Prototype|PrototypeC|Read|ReDim|Repeat|Restore|Return|Runtime|Select|EndSelect|Shared|Static|Step|Structure|EndStructure|StructureUnion|EndStructureUnion|Swap|Threaded|To|UndefineMacro|Until|UseModule|UnuseModule|Wend|While|With|EndWith|XIncludeFile|XOr|Align|Extends)\b" )
_Add_Pattern_REGEX_SHL( "CommentColor", "(;.*\r\n?)" )
_Add_Pattern_REGEX_SHL( "ConstantColor", "(\#[a-z_]\w*\$?)" )
_Add_Pattern_REGEX_SHL( "PureKeywordColor", "(\w+\$?(?=(?:\s*\.\s*\w+\s*|\s*)\())" )
_Add_Pattern_REGEX_SHL( "ASMKeywordColor", "^\s*(!.*?)(?=;|$)" )
_Add_Pattern_REGEX_SHL( "PointerColor", "((?:\@\*?|\*|\?)[a-z_]\w+(?:\$|\(\))?)" )
_Add_Pattern_REGEX_SHL( "NumberColor", "((?:\b\d+\.?\d*(?:e[+\-]?\d+)?|\$[\dabcdef]+|\%[01]+))" )
_Add_Pattern_REGEX_SHL( "StructureColor", "([a-z_]\w*(?=\s*\.[^abcdfilqsuw]))|(?![\r\n])(?<=\.)\s*([^abcdfilqsuw][a-z_]\w*)|([a-z_]\w*(?=\s*\\)(?!\r\n))|(?<=\\)\s*([a-z_]\w*)" )
_Add_Pattern_REGEX_SHL( "ModuleColor", "(\w+(?=\s*::))" )
_Add_Pattern_REGEX_SHL( "LabelColor", "(^\s*\w+\$?\s*:(?!:))" )
_Add_Pattern_REGEX_SHL( "OperatorColor", "((?:[+*/\-|!%=~]|::|&gt;|&lt;|&amp;)+)" )
_Add_Pattern_REGEX_SHL( "SeparatorColor", "([()\[\]\\,.:])" )

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
  SHL_Template("EditorFontName")  = ReadPreferenceString("EditorFontName",  "Consolas" )
  SHL_Template("EditorFontSize")  = ReadPreferenceString("EditorFontSize",  "10" )
  SHL_Template("EditorFontStyle") = ReadPreferenceString("EditorFontStyle", "" )
  SHL_Template("NormalTextColor") = ReadPreferenceString("NormalTextColor", "#FFFFFF" )
  SHL_Template("BackgroundColor") = ReadPreferenceString("BackgroundColor", "#002240" )
  
  SHL_Template("LabelColor")         = ReadPreferenceString( "LabelColor",         "<font color='#D33682'>%1</font>" )
  SHL_Template("NumberColor")        = ReadPreferenceString( "NumberColor",        "<font color='#FF628C'>%1</font>" )
  SHL_Template("StringColor")        = ReadPreferenceString( "StringColor",        "<font color='#3AD900'>%1</font>" )
  SHL_Template("ModuleColor")        = ReadPreferenceString( "ModuleColor",        "<font color='#CB4B16'>%1</font>" )
  SHL_Template("PointerColor")       = ReadPreferenceString( "PointerColor",       "<font color='#5DAEF2'>%1</font>" )
  SHL_Template("CommentColor")       = ReadPreferenceString( "CommentColor",       "<font color='#0088FF'>%1</font>" )
  SHL_Template("ConstantColor")      = ReadPreferenceString( "ConstantColor",      "<font color='#FF628C'>%1</font>" )
  SHL_Template("OperatorColor")      = ReadPreferenceString( "OperatorColor",      "<font color='#FF9D00'>%1</font>" )
  SHL_Template("SeparatorColor")     = ReadPreferenceString( "SeparatorColor",     "<font color='#FFD715'>%1</font>" )
  SHL_Template("StructureColor")     = ReadPreferenceString( "StructureColor",     "<font color='#60B66D'>%1</font>" )
  SHL_Template("ASMKeywordColor")    = ReadPreferenceString( "ASMKeywordColor",    "<font color='#6C71C4'>%1</font>" ) 
  SHL_Template("PureKeywordColor")   = ReadPreferenceString( "PureKeywordColor",   "<font color='#FFB454'>%1</font>" )
  SHL_Template("BasicKeywordColor")  = ReadPreferenceString( "BasicKeywordColor",  "<font color='#FFB054'>%1</font>" )
  SHL_Template("CustomKeywordColor") = ReadPreferenceString( "CustomKeywordColor", "<font color='#FB86CD'>%1</font>" )
  
  ClosePreferences()

EndProcedure 


;- EXAMPLE
;- ПРИМЕР

CompilerIf #PB_Compiler_IsMainFile 
  If OpenWindow(0, 0, 0, 700, 500, "", #PB_Window_SystemMenu | #PB_Window_ScreenCentered)
    
    Define PBCode$ = ""
    
    Load_INI_File_SHL( "", "" ) ; Load default style. Грузим стиль по умолчанию
    
    If ReadFile(0, #PB_Compiler_File, #PB_File_SharedRead)
      PBCode$ = ReadString(0, #PB_File_IgnoreEOL)
      CloseFile(0)
    EndIf
    
    Define HTML$ = "<head><meta charset='utf-8'></head>"+
                   "<body>"+
                   "<pre style='color:"+SHL_Template("NormalTextColor")+
                   ";background:"+SHL_Template("BackgroundColor")+";display:table;padding:10px;'>" +
                   "<code>"+PB_Syntax_Highlighting( PBCode$ )+"</code>" + 
                   "</pre>"+
                   "</body>"
    
    WebGadget(0, 10, 10, 680, 480, "")
    
    SetGadgetItemText( 0, #PB_Web_HtmlCode, HTML$ )
    
    Repeat
    Until WaitWindowEvent() = #PB_Event_CloseWindow
  EndIf

CompilerEndIf
