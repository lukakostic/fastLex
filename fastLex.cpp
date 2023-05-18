
#include<stdio.h>
#include<vector>
#include<tuple>
#include<string>
#include<algorithm>
#include<cstring>
#include<iostream>

using namespace std;

#define DBG true


#pragma region LocalFn //make local function possible (function in function)
//fix da lambda moze biti pozvana kao fn(...) umesto fn(fn,...) ali samo van lambde... unutra mora fn(fn,...) (rekurzivni poziv)
auto __fixLambda__ = [](auto&& fn){
  return [ self = std::forward<decltype(fn)>(fn)] 
         (auto&&... args) {
           return self(self, std::forward<decltype(args)>(args)...);
         };
};
#define LF(_RET_,_NAME_,...) auto _NAME_ = __fixLambda__([&](auto&& _NAME_, __VA_ARGS__) -> _RET_ {
#define LE(_RET_,_NAME_) auto _NAME_ = __fixLambda__([&](auto&& _NAME_) -> _RET_ {
#define LFE });
//use :  LF(int,f, uint64_t n){ return f(f, n); }LFE
// usual lambda:   auto fn = [&](auto&& fn, int n ) -> int { return fn(fn,n); };
// da van lambde ne mora zovemo fn(fn, n) nego fn(n) mi uradimo __fixLambda__ ovako:  auto fn = fix( ..lambda.. ); 
#pragma endregion LocalFn

struct Enum2{ // nested enum kinda, has 2 parts: main and sub
    uint16_t v; // 2 uint8_t: main and sub.
    inline uint8_t main() const{ return v & 0xFF; }
    inline uint8_t sub() const{ return (v>>8) & 0xFF; }
    inline bool is(const Enum2 b) const{ return (v&0xFF)==(b&0xFF); } //do main parts match

    constexpr Enum2(const uint16_t m,const  uint16_t s){ v = (s<<8) | m; };
    constexpr Enum2(uint16_t vv) :v(vv){};
    constexpr operator uint16_t() const { return v; }
};



/******************************
Token - whitespace, operator or identifier, or literal.
We have a list of ALL tokens and a list of PARSED tokens (indexes from list of all tokens)
As new tokens get encountered (literal) we push it to ALL tokens and to PARSED (assuming new was discovered during parsing)

Parser goes letter by letter, if it encounters a definite operator it does a split (leading identifier + discovered operator/whitespace)
Once the parser finds a letter thats the first letter of a splitting operator, it goes forward until it confirms it can only be that operator.
If the current letter isnt a part of any operator (starting), then we 


preferans dugih operatora over krace. Znaci ako imamo + i ++ mi ce ++ parsovati kao ++ ne +

******************************/
//////////////////////////////////////// Helper methods to iterate basic[] arrays since c/c++ isnt a normal language.
// token detail, <comma separated token strings>
#define ___pushTokens(X,...) \
    {const char* ARRAY[] = {__VA_ARGS__};\
    for(int II = 0; II < sizeof(ARRAY)/sizeof(char*); II++)\
    all.push_back(LexToken(LexToken::X,string(ARRAY[II])));}
// <comma separated identifier strings>
#define ___pushIdents(...) \
    {const char* ARRAY[] = {__VA_ARGS__};\
    for(int II = 0; II < sizeof(ARRAY)/sizeof(char*); II++)\
    idents.push_back(string(ARRAY[II]));}
////////////////////////////////////////

struct LexToken{
    enum LexTokenDetail : uint16_t{
        None = Enum2{0,0},
        
        Whitespace = Enum2{1,0}, 
        WhitespaceNewline = Enum2{1,1},
        
        _Str = Enum2{2,0}, 
        StrQuote = Enum2{2,1}, 
        StrQuoteD = Enum2{2,2}, 
        StrQuoteTick = Enum2{2,3}, 
        StrQuoteTTick = Enum2{2,4},

        _Comment = Enum2{3,0},
        CommentSingle = Enum2{3,1}, 
        CommentMOpen = Enum2{3,2}, 
        CommentMClose = Enum2{3,3}
    } detail; //type of token (affects parsing)
    string str;            //symbol of token
};

struct Parsed{ //for list of all parsed things (tokens and identifiers)
    enum ParsedType {
        Identifier,
        Operator,
        Number,
        String
    } type;
    uint16_t idx; //index of token or identifier

    //additional info for parsed token:
    union ParsedInfo{
        enum strInfo: uint8_t{
            Quote, QuoteD, QuoteTick, QuoteTTick
        } stringType;
    } info;
};


struct fastLex{
    static constexpr int STR_LITERAL_BUF_SIZE = 1024; //starting size, its dynamic
    struct fastLexHelper{
        enum LexerState : uint16_t{
            Normal = Enum2{1,0},

            Comment = Enum2{2,0},
            CommentSingleLine = Enum2{2,1},
            CommentMultiLine = Enum2{2,2},
            
            NumberLiteral = Enum2{3,0},
            
            StringLiteral = Enum2{4,0},
            StringLiteralQuote = Enum2{4,1},
            StringLiteralDQuote = Enum2{4,2},
            StringLiteralTick = Enum2{4,3},
            StringLiteralTTick = Enum2{4,4},
        } state = LexerState::Normal;
        fastLex* lex;

        void buildTree(){
            ////Lexically order tokens (1.size, 2.lex. order)
            std::sort(begin(lex->all), end(lex->all),
            []( const auto& lhs, const auto& rhs ){
                return (lhs.str.size()==rhs.str.size()) ? (lhs.str > rhs.str) : (lhs.str.size()<rhs.str.size());
            });
        }
    }* _ = nullptr; //runtime stuff but you dont need once finished
    
    vector<LexToken> all; //all tokens (operators)
    vector<string> idents; //all identifiers (non splitting tokens)
    
    vector<Parsed> parsed; //list of all parsed things (identifiers and tokens) <isIdent, index>


    void init(){
        if(_!=nullptr) delete _;
        _ = new fastLexHelper();
        _->lex = this;
        _->buildTree();
    }
    void free(){
        if(_!=nullptr) delete _;
    }
    void __populateCppTokens(){ //some example tokens
        ___pushIdents("new","free","delete","for","while","return","break","continue","else","elif","try","finally","switch","case");

        ___pushTokens(Whitespace," ","\t");
        ___pushTokens(WhitespaceNewline, "\n","\r","\0");
        ___pushTokens(None,
            "{","}","(",")","[","]","<[","]>",
            ".",",",":","::",";","?","\\","##","$",
            "<",">","/","*","%","+","-","=","++","--","!","&","|","&&","||",">>","<<",">=","<=","<<<",">>>","***","==","===","!=","+=","-=","^","^=","/=","*=","%=",
            ".=",".?","?.","??","?:","?,"
        );
        
        ___pushTokens(StrQuote,"'");
        ___pushTokens(StrQuoteD,"\"");
        ___pushTokens(StrQuoteTick,"`");
        ___pushTokens(StrQuoteTTick,"```");

        ___pushTokens(CommentSingle,"//");
        ___pushTokens(CommentMOpen,"/*");
        ___pushTokens(CommentMClose,"*/");
    }

    void parse(char* str){
        /*
        in special mode (state != Normal, say String or Number or comment)
        we are wating for an ending token - token to end current state and go back to normal.
        Numbers are non special mode , they have no ending character (are digits).
        Numbers are parsed if theres nothing behind them in string - since operators are split immediately and identifiers arent.
        We check for number mode if current string is empty and we are getting a number digit.
        */
        static bool specialMode = false; 
        uint16_t endingToken = 0; //token we end spec. mode at.
        //~~~~~~~~~ String parsing:
        vector<char> stringLiteralBuffer;
        char pc = '\0'; //previous char , to see if escaped

        ////////////////////////////////////////////////
        parsed.clear();
        //current word start and end (string view start and end)
        
        struct stringView{
            char* str=nullptr;
            int s=-1,e=-1;
            string mkStr(){
                string ss = ""; 
                if(str==nullptr) return ss;
                for(int c = s; c<e; c++) ss.push_back(str[c]);
                return ss;
            }
            void print(){
                if(str==nullptr) return;
                for(int c = s; c<e; c++) printf("%c",str[c]);
            }
            void move(int ns, int ne){ s=ns;e=ne; }
            void step(){ e++; }
            void jump(){ s++; e++; }
            int length(){ return max(e-s,0); }
            char last(){ return str[e-1]; }
            stringView clone(){
                return stringView{str,s,e};
            }
        } sv = {str,0,1};

        struct tokenEncounter{
            uint16_t idx;
            uint8_t charCnt;
            uint16_t charIdx;
        };
        vector<pair<uint16_t,uint16_t>> completed;
        vector<tuple<uint16_t,uint8_t,uint16_t>> found;   //u toku parsovanja, ako smo tipa u prvom karakteru X ili trecem Y mi stavimo ih kao i broj slova. 
        //             <idx, charCnt, charIdx>

        //we always look at last char of stringView. we step one by one char. If we find anything, we cause a split (move stringView start, remove potentials)
        //if we ever have a fully completed match, and no other lengthier match that contains that match, we can split. If we do have one that contains it, we run fowrad to see if any.
        
        //as we find first leter encounters, we add them, but thats the last part.
        //first,  If we already have encounters and they still match, we increment their char count
        // if any of those encounters get finished, we add it to 'finished' array. We split and discard any existing matches.

        LE(void,pushIdent) parsed.push_back(Parsed{Parsed::ParsedType::Identifier,(uint16_t)(idents.push_back(sv.mkStr()),idents.size()-1)}); LFE
        LF(void,pushOperator,int idx){ //switch state point
            auto det = (Enum2) all[idx].detail;
            
            if(specialMode){
                if(det == endingToken){
                    bool isStr = ((Enum2)_->state).is(fastLexHelper::LexerState::StringLiteral);
                    if(isStr && pc=='\\') return; //its an escaped token..
                    _->state = fastLexHelper::LexerState::Normal;
                    specialMode = false;
                    if(isStr){
                        stringLiteralBuffer.push_back('\0');
                        string s = stringLiteralBuffer.data();
                        if(DBG) cout<<"str:"<<s<<endl;
                        parsed.push_back(Parsed{Parsed::String,(uint16_t)(idents.push_back(s),idents.size()-1), (Parsed::ParsedInfo::strInfo)(det.sub()-1) });
                    }
                }else throw "Passed token that isnt ending token.. how?";
                return;
            }

            if(_->state==fastLexHelper::LexerState::Normal){
                if(det.is(LexToken::_Str)){
                    specialMode = true;
                    endingToken = det;
                    
                    if( det == LexToken::StrQuoteD) _->state = fastLexHelper::LexerState::StringLiteralDQuote;
                    else if( det == LexToken::StrQuote)  _->state = fastLexHelper::LexerState::StringLiteralQuote;
                    else if( det == LexToken::StrQuoteTick) _->state = fastLexHelper::LexerState::StringLiteralTick; 
                    else if( det== LexToken::StrQuoteTTick) _->state = fastLexHelper::LexerState::StringLiteralTTick;
                    if(DBG) cout<<"STR ["<<sv.mkStr()<<"]"<<endl;
                    //splitOne();
                    //sv.jump();
                    stringLiteralBuffer.clear();
                    stringLiteralBuffer.shrink_to_fit();
                    stringLiteralBuffer.reserve(STR_LITERAL_BUF_SIZE);
                    pc = '\0';
                    return;
                }else if(det == LexToken::CommentSingle){
                    specialMode = true;
                    endingToken = LexToken::WhitespaceNewline;
                    _->state = fastLexHelper::LexerState::CommentSingleLine;
                    return;
                }else if(det == LexToken::CommentMOpen){
                    specialMode = true;
                    endingToken = LexToken::CommentMClose;
                    _->state = fastLexHelper::LexerState::CommentMultiLine;
                    return;
                }
            }

            parsed.push_back(Parsed{Parsed::Operator,(uint16_t)idx}); 
        }LFE
        LE(void,pushNumber) parsed.push_back(Parsed{Parsed::Number,(uint16_t)(idents.push_back(sv.mkStr()),idents.size()-1)}); LFE
        //LE(void,pushString) parsed.push_back(Parsed{ParsedType::String,(uint16_t)(idents.push_back(sv.mkStr()),idents.size()-1)}); LFE
        
        // on split we discard potenetials vector and 
        LF(void,split,int completedIdx){ //when the string contains both ident. and oper. "identifier<operator>"
            if(DBG) cout << "SPLIT!! '";
            sv.e = completed[completedIdx].second;
            if(DBG) sv.print();
            if(DBG) cout <<"' + '" << all[completed[completedIdx].first].str<<"'"<<endl;
            
            if(specialMode){
                pushOperator(completed[completedIdx].first);
            }else{
                if(sv.length()!=0)
                    pushIdent();
                if(!((Enum2)all[completed[completedIdx].first].detail).is(LexToken::Whitespace))
                //////////////////Are we sure its an operator?
                    pushOperator(completed[completedIdx].first);
            }

            sv.e = sv.s = completed[completedIdx].second + all[completed[completedIdx].first].str.size();

            completed.clear();
            found.clear();
        }LFE

        LE(void,splitOne){ //when the string contains only "<operator>" and not "identifier<operator>", so we have nothing to split we just switch mode..
            if(DBG) cout << "SPLIT ONE '";
            if(completed.size()>0){ //mozda smo imali neke operatore a ne samo identifier: "a="
                if(completed.size()>1) throw "Completed size > 1 in split now";
                split(0);
            }else{
                sv.e--;
                if(DBG) sv.print();
                if(DBG) cout <<"'"<<endl;
                
                if(sv.length()!=0){
                    if(_->state == fastLexHelper::LexerState::Normal)
                        pushIdent();
                    else if(_->state == fastLexHelper::LexerState::NumberLiteral)
                        pushNumber();
                    else if(Enum2(_->state).is(fastLexHelper::LexerState::StringLiteral))
                        throw "Pushing string?";//pushString();
                }

                sv.s = sv.e;

                completed.clear();
                found.clear();
            }
        }LFE

        LF(void,step, char c){ 
            //append one char, end those who dont fit
            for(int i = 0; i<found.size(); i++){
                auto&& [ idx, charCnt, charIdx ] = found[i];
                if(all[idx].str[charCnt]==c)
                    charCnt++;
                else
                    found.erase(found.begin()+(i--));
            }
            //add newly founds
            for(int i = 0; auto& t : all){
                if(t.str[0]==c && (specialMode==false || all[i].detail == endingToken))
                    found.emplace_back(i,1,sv.e-1);
                i++;
            }
            //count how many finished fully..
            for(int i = 0; i<found.size(); i++){
                auto&& [ idx, charCnt, charIdx ] = found[i];
                if(all[idx].str.length()==charCnt){
                    completed.emplace_back(idx,charIdx);
                    found.erase(found.begin()+(i--));
                }
            }
            //operate on finished ones (maybe we SPLIT!)
            if(completed.size()>0){
                if(completed.size()==1){
                    if(found.size()==0)
                        split(0); //yay only one match, we can split!
                    else //multiple starting sam remaining but only 1 completed
                        if(DBG) cout<<"One completed but more found. Cant decide yet! "<<endl;
                    
                }else{  //multiple completed (some old some new)
                        if(DBG) cout<<"Multiple completed:"<<endl;
                        if(DBG) for(auto k : completed) cout<<all[k.first].str<<endl;
                        //Find oldest but biggest one!
                        int maxIdx = 0, maxSize = all[completed[maxIdx].first].str.size();
                        for(int i = 1; i < completed.size(); i++){
                            if(completed[maxIdx].second != completed[i].second) break; //we moved forwards..
                            int is = all[completed[i].first].str.size();
                            if(is > maxSize){
                                maxSize = is;
                                maxIdx = i;
                            }
                        }
                        //found max length/oldest:
                        split(maxIdx);
                }
            }
        }LFE
        

        LF(void,processChar_Number, char c){
            if(!isdigit(c)&&c!='.'){
                splitOne();
                _->state = fastLexHelper::LexerState::Normal;
                return;
            }
        }LFE
        

        LF(void,processChar_String, char c){
            step(c);

            if(specialMode && ((Enum2)_->state).is(fastLexHelper::LexerState::StringLiteral) && c!='\\'){ 
                 //are we still working or did we split (encountered closing quote, left string parsing)
                char ch = c;
                if(pc=='\\'){
                    if(c == 'n') ch = '\n';
                    if(c == 'r') ch = '\r';
                    if(c == 't') ch = '\t';
                    if(c == 'v') ch = '\v';
                    if(c == 'f') ch = '\f';
                    if(c == 'b') ch = '\b';
                    if(c == '?') ch = '\?';
                    if(c == 'a') ch = '\a';
                    if(c == '\\') ch = '\\';
                    if(c == '0') ch = '\0';
                    if(c == '"') ch = '"';
                    if(c == '\'') ch = '\'';
                    if(c == '`') ch = '`';
                }
                stringLiteralBuffer.push_back(ch);
            }

            pc = c;
        }LFE


        LF(void,processChar_Normal, char c){
            //number literal entry check
            if(sv.length()==1 && isdigit(c)){
                splitOne();
                _->state = fastLexHelper::LexerState::NumberLiteral;
                return;
            }

            step(c);
        }LFE
        
        LF(void,processChar, char c){ //calls appropriate state fn handler
            if(DBG) cout << endl << "{"<< c<<"}" << endl;
            switch(_->state){
                case fastLexHelper::LexerState::Normal:
                    return processChar_Normal(c);
                case fastLexHelper::LexerState::NumberLiteral:
                    return processChar_Number(c); 
                case fastLexHelper::LexerState::StringLiteralQuote:
                case fastLexHelper::LexerState::StringLiteralDQuote:
                case fastLexHelper::LexerState::StringLiteralTick:
                    return processChar_String(c);
                case fastLexHelper::LexerState::CommentSingleLine:
                case fastLexHelper::LexerState::CommentMultiLine:
                    return step(c);
            }
        }LFE

        for(;sv.last()!='\0';sv.step()) processChar(sv.last()); //march all chars   
        for(int i = 5;i-->0;) processChar('\0'); //just to end it off and ensure all operators parsed
    }
};
