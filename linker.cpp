#include<stdio.h>
#include<string.h>
#include<iomanip>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream> 
#include <unordered_set>
#include<set>
#include <map>
#include<algorithm>
#include <iterator>
#include<tuple> 
#include<vector>
#include<math.h>
#include<string.h>

using namespace std;

class symval {       // The class
  public:             // Access specifier
    int val;        // Attribute (int variable)
    string error;  // Attribute (string variable)
    bool used;
    int mno;
};

ifstream myfile;
char delims[] = " \n\t";
vector<string> cur_warnings;
vector<string> cur_symbols;
vector<symval> cur_use_list;
vector<string> cur_def_list;
vector<string> order;
// ifstream myfile ("input-1");
string line;
int line_no = 0;
int lineoffset=1;
char* prev_tok=NULL;
char* l;
int module_no = 0;
int instr_count = 0;
int cur_base_addr = 0;
int cur_module_instr_count = 0;
map<string,symval> symboltable;

// set<char> addmodes = {'I','E','A','R'};

map<string, symval>::iterator itr;
vector<string> :: iterator s_itr;

void print_table()
{

  cout<<"Symbol Table\n";
for (int i = 0; i<order.size(); i++) {
  itr = symboltable.find(order[i]);
        cout << itr->first<< '=' << itr->second.val <<" "<<itr->second.error  << '\n';
    }


}


void __parseerror(int errcode) {   //__parseerror()
 const char* errstr[] = {
 "NUM_EXPECTED", // Number expect, anything >= 2^30 is not a number either
 "SYM_EXPECTED", // Symbol Expected
 "ADDR_EXPECTED", // Addressing Expected which is A/E/I/R
 "SYM_TOO_LONG", // Symbol Name is too long
 "TOO_MANY_DEF_IN_MODULE", // > 16
 "TOO_MANY_USE_IN_MODULE", // > 16
 "TOO_MANY_INSTR", // total num_instr exceeds memory size (512)
 };
 printf("Parse Error line %d offset %d: %s\n", line_no, lineoffset, errstr[errcode]);
}





char* next_token(char* ptr)
{ 
  if(ptr!=NULL)
  {   int last_tok_len = strlen(prev_tok);
      prev_tok = strtok(NULL,delims);
      if(prev_tok == NULL){
        int oldline = line_no;
        // int oldoff = lineoffset;
        int oldoff = line.length()+1;

        //getline (myfile,line);
	if(!getline(myfile,line))
       // if(myfile.eof() && line.empty())
      {line_no = oldline;
        lineoffset = oldoff;
         return NULL;}

        line_no ++;
        lineoffset = line.length()+1;
        

        while(line.empty() || line.find_first_not_of(" \t") == std::string::npos)
      {
        oldoff = lineoffset;
        oldline = line_no;
        line_no ++;
        getline (myfile,line);
      if(myfile.eof())
      {line_no = oldline;
        lineoffset = oldoff;
        break;}
        last_tok_len = 0;
        lineoffset = line.length()+1;
      }
      l = new char[line.length()+1];
      strcpy(l, line.c_str());
      prev_tok = strtok(l,delims);
      if(prev_tok ==NULL)
        {return NULL;}
               
      }
      lineoffset = prev_tok - l + 1; 
      return prev_tok;
    
  }
  else
  {
      getline (myfile,line);
      int oldline = line_no;
      int oldoff = lineoffset;
      while(line.empty() || line.find_first_not_of(" \t") == std::string::npos )
      { 
        line_no ++;
        getline (myfile,line);
        lineoffset = 1;
        if(myfile.eof())
          {break;}

      }
      if(myfile.eof())
        {line_no--;}
      line_no ++;
      lineoffset = 1;
      l = new char[line.length()+1];
      strcpy(l, line.c_str());
      prev_tok = strtok(l,delims);
      if(prev_tok == NULL){
        line_no = oldline;
        lineoffset = oldoff;
        return NULL;
      }
      lineoffset = prev_tok - l + 1; 
      return prev_tok;
  }
}

void createModule(){
  module_no++;
  cur_base_addr = cur_base_addr + cur_module_instr_count;
  cur_module_instr_count = 0;
  cur_symbols.clear();
  cur_warnings.clear();


}

int readInt(int start)
{
  prev_tok = next_token(prev_tok);
  if(prev_tok == NULL and start)
    {
      
      
      return -99;}
  else if(prev_tok == NULL and !start){
    __parseerror(0);
    exit(0);
  } //raise error
  for(int i = 0; i<strlen(prev_tok); i++)
  {
    if(!isdigit(prev_tok[i])){
      __parseerror(0);
    exit(0); //raise error
    }
  } 
  if(atoi(prev_tok)>=pow(2,30))
  {__parseerror(0);
    exit(0); }//raise error}
  return atoi(prev_tok);

}


string readSym()
{
prev_tok = next_token(prev_tok);

if(prev_tok == NULL)
{ __parseerror(1);
    exit(0);} //raise error


if(strlen(prev_tok)>16)
  {__parseerror(3);
    exit(0); }//raise error


  for(int i = 0; i<strlen(prev_tok); i++)
  {
    if(i==0 and !isalpha(prev_tok[i])){
      __parseerror(1);
    exit(0);//raise error
    }
    else if(i>0 and (!isalpha(prev_tok[i]) and !isdigit(prev_tok[i])))
    {
     __parseerror(1);
    exit(0);//raise error 
    }
  }
  string str(prev_tok);
  return str;

}

void createSymbol(string sym,symval val){

if ( symboltable.find(sym) != symboltable.end() ) {
  std::map<string,symval>::iterator itr_temp;
  itr_temp = symboltable.find(sym);
  itr_temp->second.error = "Error: This variable is multiple times defined; first value used";
 //raise error


} else {
symboltable.insert(pair<string,symval>(sym,val));
order.push_back(sym);
  
}

}

char readIEAR()
{
  prev_tok = next_token(prev_tok);
  if(prev_tok == NULL)
    {__parseerror(2);
    exit(0);} //raise error
  if(strlen(prev_tok)!=1){
    __parseerror(2);
    exit(0); //raise error
  }
  // if(addmodes.find(prev_tok[0]) == addmodes.end())
  if(!(prev_tok[0] == 'I' || prev_tok[0] == 'E' || prev_tok[0] == 'A' || prev_tok[0] == 'R' ))

  {
    __parseerror(2);
    exit(0);
  }
  return prev_tok[0];

}



int main (int argc, char **argv) {
  myfile.open(argv[1]);
    
    if(myfile.is_open())
    {
    while ( !myfile.eof() )
    { 

      createModule();
      

      int def_count = readInt(1);
//  cout<<def_count<<endl;
      if(def_count==-99){
print_table();
        break;}
      // cout<<def_count<<endl;
      if(def_count>16){
        __parseerror(4);
    exit(0);
      }
      for(int i = 0; i<def_count; i++)
      {
          string sym = readSym();
          int val = readInt(0);
          val = val+cur_base_addr;
          symval val_obj;
          val_obj.error = "";
          val_obj.val = val;
          val_obj.used = false;
          val_obj.mno = module_no;
          createSymbol(sym,val_obj);
          cur_symbols.push_back(sym);

      }

      int usecount = readInt(0);
      if(usecount>16){
        __parseerror(5);
    exit(0);
      }
  //     cout<< usecount<<endl;
      for(int i =0; i<usecount; i++)
      {
        string sym = readSym();
      }
      cur_module_instr_count = readInt(0);
      if(cur_module_instr_count+cur_base_addr>512)
      {
        __parseerror(6);
    exit(0);

      }
      // cout<<cur_module_instr_count<<endl;
      for(int i = 0; i<cur_module_instr_count; i++)
      {
        char addressmode = readIEAR();
        int operand = readInt(0);
       



      }

      for(string s: cur_symbols){
        itr = symboltable.find(s);
        if(itr->second.val>=cur_base_addr+cur_module_instr_count){
          printf("Warning: Module %d: %s too big %d (max=%d) assume zero relative\n",module_no,s.c_str(),itr->second.val-cur_base_addr,cur_module_instr_count-1) ;
          itr->second.val = cur_base_addr;

        }
      }
    //   for (itr = symboltable.begin(); itr != symboltable.end(); ++itr) {
    //     cout << '\t' << itr->first
    //          << '\t' << itr->second.val <<" "<<itr->second.error  << '\n';
    // }

      // prev_tok = next_token(prev_tok);
      // if(prev_tok!=NULL){cout<<"Token "<<line_no<<":"<<prev_tok-l+1<<" "<<prev_tok<<endl;}    
  
    }

    // print_table();
    cout<<endl;
    cout<<"Memory Map"<<endl;
    myfile.close();
    
    }else{
      cout<<"Unable to open file";
    }






    //++++++++++++++++++++****************************#######################  PASS 2   #################################*****************************+++++++++++++++++++
      
module_no = 0;
cur_base_addr = 0;
cur_module_instr_count = 0;
cur_symbols.clear();
cur_warnings.clear();
 myfile.open(argv[1]);
int counter = 0;

if(myfile.is_open())
    {
    while ( !myfile.eof() )
    {

      createModule();
      cur_use_list.clear();
      cur_def_list.clear();

      int def_count = readInt(1);
      if(def_count==-99){break;}
      
      for(int i = 0; i<def_count; i++)
      {
          string sym = readSym();
          int val = readInt(0);
          
      }

      int usecount = readInt(0);
     
      // cout<< usecount<<endl;
      for(int i =0; i<usecount; i++)
      {
        string sym = readSym();
        symval val_obj;
          val_obj.error = sym;
          val_obj.val = 0;
          val_obj.used = false;
          val_obj.mno = -99;
        cur_use_list.push_back(val_obj);
        // itr = symboltable.find(sym)
        // itr->second.used = true;

      }
      cur_module_instr_count = readInt(0);
    
      // cout<<cur_module_instr_count<<endl;
      for(int i = 0; i<cur_module_instr_count; i++)
      {
        char addressmode = readIEAR();
        int operand = readInt(0);

        int opcode = operand/1000;
        int oper = operand%1000;
        if(addressmode == 'I' and operand>=10000){
          cout<<setw(3)<<setfill('0')<<counter<<": 9999 Error: Illegal immediate value; treated as 9999"<<endl; 
        }
        else if(addressmode == 'E' and operand>=10000){
          cout<<setw(3)<<setfill('0')<<counter<<": 9999 Error: Illegal opcode; treated as 9999"<<endl; 
        }
        else if(addressmode == 'A' and operand>=10000){
          cout<<setw(3)<<setfill('0')<<counter<<": 9999 Error: Illegal opcode; treated as 9999"<<endl; 
        }
        else if(addressmode == 'R' and operand>=10000){
          cout<<setw(3)<<setfill('0')<<counter<<": 9999 Error: Illegal opcode; treated as 9999"<<endl; 
        }else{

switch(addressmode) {
    case 'I' : cout<<setw(3)<<setfill('0')<<counter<<": " <<setw(4)<<setfill('0') <<operand<<endl;
             break;       // and exits the switch
    case 'E' : //cout<<"OPERRRRRRRR:   "<<oper<<endl;
                if(oper>usecount-1)
                  {cout<<setw(3)<<setfill('0')<<counter<<": " <<setw(4)<<setfill('0') <<operand<<" Error: External address exceeds length of uselist; treated as immediate"<<endl;}
                else{
                  // cout<<cur_use_list[oper].error<<"@@@@@@@"<<endl;
                  itr = symboltable.find(cur_use_list[oper].error);
                  if(itr == symboltable.end()){
                    cout<<setw(3)<<setfill('0')<<counter<<": " <<setw(4)<<setfill('0') <<opcode*1000<<" "<<"Error: " +cur_use_list[oper].error+" is not defined; zero used"<<endl;
                    cur_use_list[oper].used = true;
                    itr->second.used = true;
                  }
                  else
                  {//cout<<"oper:  "<<oper<<"  "<<cur_use_list.size()<<endl;
                    cout<<setw(3)<<setfill('0')<<counter<<": " <<setw(4)<<setfill('0') <<opcode*1000+(itr->second.val)<<endl;
                    cur_use_list[oper].used = true;
                    itr->second.used = true;
                  }
                }
             break;
    case 'A':  if(oper>=512){
                cout<<setw(3)<<setfill('0')<<counter<<": "<<setw(4)<<setfill('0')<<opcode*1000<<" "<<"Error: Absolute address exceeds machine size; zero used"<<endl;
          }
          else{ cout<<setw(3)<<setfill('0')<<counter<<": "<<setw(4)<<setfill('0')<<operand<<endl;
   }break;
    case 'R':
      if(oper > (cur_module_instr_count-1)) {
                 cout<<setw(3)<<setfill('0')<<counter<<": " <<setw(4)<<setfill('0') <<opcode*1000+cur_base_addr<<" "<<"Error: Relative address exceeds module size; zero used"<<endl;
            }
            else {
                cout<<setw(3)<<setfill('0')<<counter<<": " <<setw(4)<<setfill('0')<<operand+cur_base_addr<<endl;
            }

    break;
}

        }

        
counter++;



      }

      for(symval s: cur_use_list){
        // cout<<s.used<<endl;
        if(!s.used){
          cout << "Warning: Module " << module_no <<": "<< s.error<<" appeared in the uselist but was not actually used"<<endl;

        }
      }
      

  
    }
    cout<<endl;
    for (int i = 0; i<order.size(); i++) {
	itr = symboltable.find(order[i]);
      if(!itr->second.used){
        cout << "Warning: Module " << itr->second.mno <<": "<< itr->first<<" was defined but never used\n";
      }
        
    }
 
    myfile.close();
    
    }

    else{
      cout<<"Unable to open file";
    }





  return 0;
    }




 
