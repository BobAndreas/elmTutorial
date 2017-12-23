import Html exposing ( .. )
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Char exposing ( .. )


main : Program Never Page Msg
main = 
    Html.beginnerProgram { page = page, view = view, update = update}


model : PModel
model = PModel NotLoggedIn

page : Page
page = MainPage model


-- MODEL
type Page = MainPage PModel| LoginPage LModel| SignUpPage SUModel

--Login Model
type alias LModel = 
    {   name : String
    ,   password : String
    ,   error : LError
    }
type LError
    = NoLoginError
    | LoginFailed
    
-- Page Model
type alias PModel =
    { loggedIn : LoginState
    }

type LoginState = NotLoggedIn | LoggedIn Person

type alias Person =
    { name : String
    , age : Int
    , id : Int
    }
--SignUp Model
type alias SUModel = 
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , error : SUError
    }
type SUError = NoSUError | Existing SUErrorType SUError

type SUErrorType 
    = PwToShort 
    | PwDontMatch 
    | NameTooLong 
    | AgeNaN 
    | PwNotSafe


possibleSUErrors : List ((SUModel->Bool), SUErrorType)
possibleSUErrors = [
    ((\model -> String.length model.password < 8), PwToShort)
    ,((\model -> model.password /= model.passwordAgain), PwDontMatch)
    ,((\model -> String.length model.name > 20), NameTooLong)
    ,((\model -> case String.toInt model.age of 
                    Result.Err _ -> True
                    otherwise -> False), AgeNaN
                )
    ,((\model -> not (String.any Char.isUpper model.password) || not (String.any Char.isLower model.password) || not (String.any Char.isDigit model.password)), PwNotSafe)
    ]
defaultWarning : SUErrorType -> String
defaultWarning errorType = 
    case errorType of
        PwToShort -> "PasswordToShort"
        PwDontMatch -> "Password and confirmation don't match"
        NameTooLong -> "Name is too long"
        AgeNaN -> "Age is not a whole number"
        PwNotSafe -> "Passwords should contain lower & upper case as well as numbers"




--UPDATE

type Msg 
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit
    | LoginReq
    | SignUpReq
    | LogOutReq


update : Msg -> Page -> Page
update msg page =
    case page of
        SignUpPage model -> case msg of
                    Name newName -> {model | name = newName}
                    Password newPW -> {model | password = newPW}
                    PasswordAgain newPW -> {model | passwordAgain = newPW}
                    Age newAge -> {model | age = newAge}
                    Submit -> {model | error = checkModel model}
                    otherwise -> page
        LoginPage model -> case msg of
                            Name name -> LoginPage {model | name = name}
                            Password pw -> LoginPage {model | password = pw}
                            Submit ->   let id = getID model.name model.password
                                        in case id of
                                        Just val -> MainPage (PModel LoggedIn (Person model.name (getAge val) val))
                                        Nothing -> LoginPage {model | error = LoginFailed}
                            otherwise -> page 
        MainPage model -> case msg of
                            LoginReq -> LoginPage "" "" NoLoginError
                            SignUpReq -> SignUpPage "" "" "" "" NoSUError
                            otherwise -> page

checkModel : SUModel -> SUError
checkModel model = 
    List.foldl (\(condition, errorType) prevError -> 
        if condition model
        then Existing errorType prevError 
        else prevError) NoSUError possibleSUErrors


--VIEW

view : Page -> Html Msg
view page =
    case page of
        SignUpPage model ->
            div[] 
                [input[ type_ "text", placeholder "Name", onInput Name] []
                , showDefaultWarning NameTooLong model.error
                , div[] 
                    [input[ type_ "password", placeholder "Password", onInput Password] [] 
                    , showDefaultWarning PwToShort model.error
                    , showDefaultWarning PwNotSafe model.error
                    ]        
                , div[] [input[ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain] [] 
                    , showDefaultWarning PwDontMatch model.error
                    ] 
                , div[] [input[ type_ "Age", placeholder "Enter your age", onInput Age] [] 
                    , showDefaultWarning AgeNaN model.error
                    ]    
                , button [(onClick Submit)] [text "submit"]
                ]
        LoginPage model ->
            div[]
                [ input[type_"text", placeholder "Name", onInput Name][]
                , input [type_ "password", placeholder "Password", onInput Password]
                , button [(onClick Submit)] [text "Login"]
                ]
        MainPage model ->
            let showContent = showRestrictive model.loggedIn
                content1 = div[][textarea "Bullshit content1"]
                content2 = div[][textarea "Fake news"]
            in div[]
                [ showLogin model.loggedIn
                , showContent content1
                , showContent content2
                ]
showRestrictive : LoginState -> Html Msg -> Html msg
showRestrictive state content = case state of
                    NotLoggedIn -> div[][ textarea "Login or signup to see this wonderful content"]
                    LoggedIn _ -> content

showLogin : LoginState -> Html Msg
showLogin state = case state of
                NotLoggedIn -> div[]
                    [ textarea "Not Signed In"
                    , button [(onClick SignUpReq)] [text "Sign Up"]
                    , button [(onClick LoginReq)] [text "Login"]
                    ]
                LoggedIn person -> div[]
                    [textarea "Logged in as" ++ person.name
                    ,button[(onClick LogOutReq)][text "Log out"]
                    ]

showWarning : SUErrorType -> SUError -> String -> Html msg
showWarning etype error msg =
    case error of
        Existing cur next -> if cur == etype
                             then div [style [("color", "red")]][text msg]
                             else showWarning etype next msg   
        NoSUError -> div[][]  

showDefaultWarning: SUErrorType->SUError->Html msg
showDefaultWarning errorType error = showWarning errorType error (defaultWarning errorType)

--WebSocket
listen : String -> (String -> msg) -> Sub msg
listen string func = func stringProperty



