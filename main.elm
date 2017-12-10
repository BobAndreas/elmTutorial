import Html exposing ( .. )
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main : Program Never Model Msg
main = 
    Html.beginnerProgram { model = model, view = view, update = update}

-- MODEL

type alias Model = 
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , error : Error
    }


model : Model
model = Model "" "" "" "" NoError


type ErrorType = PwToShort | PwDontMatch | NameTooLong | AgeNaN

possibleErrors : List ((Model->Bool), ErrorType)
possibleErrors = [
    ((\model -> String.length model.password < 8), PwToShort)
    ,((\model -> model.password /= model.passwordAgain), PwDontMatch)
    ,((\model -> String.length model.name > 20), NameTooLong)
    ,((\model -> case String.toInt model.age of 
                    Result.Err _ -> True
                    otherwise -> False), AgeNaN
                )
    ]
defaultWarning : ErrorType -> String
defaultWarning errorType = 
    case errorType of
        PwToShort -> "PasswordToShort"
        PwDontMatch -> "Password and confirmation don't match"
        NameTooLong -> "Name is too long"
        AgeNaN -> "Age is not a whole number"

type Error = NoError | Existing ErrorType Error


--UPDATE


type Msg 
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name newName -> {model | name = newName}
        Password newPW -> {model | password = newPW}
        PasswordAgain newPW -> {model | passwordAgain = newPW}
        Age newAge -> {model | age = newAge}
        Submit -> {model | error = checkModel model}

checkModel : Model -> Error
checkModel model = 
    List.foldl (\(condition, errorType) prevError -> 
        if condition model
        then Existing errorType prevError 
        else prevError) NoError possibleErrors


--VIEW

view : Model -> Html Msg
view model = 
    div[] 
        [input[ type_ "text", placeholder "Name", onInput Name] []
        , showDefaultWarning NameTooLong model.error
        , div[] 
            [input[ type_ "password", placeholder "Password", onInput Password] [] 
            , showDefaultWarning PwToShort model.error
            ]        
        , div[] [input[ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain] [] 
            , showDefaultWarning PwDontMatch model.error
            ] 
        , div[] [input[ type_ "Age", placeholder "Enter your age", onInput Age] [] 
            , showDefaultWarning AgeNaN model.error
            ]    
        , button [(onClick Submit)] [text "submit"]
        ]

showWarning : ErrorType -> Error -> String -> Html msg
showWarning etype error msg =
    case error of
        Existing cur next -> if cur == etype
                             then div [style [("color", "red")]][text msg]
                             else showWarning etype next msg   
        NoError -> div[][]  

showDefaultWarning: ErrorType->Error->Html msg
showDefaultWarning errorType error = showWarning errorType error (defaultWarning errorType)