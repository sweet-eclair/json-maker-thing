const fs = require('fs');
const yargs = require('yargs/yargs')
const { hideBin } = require('yargs/helpers')
const argv = yargs(hideBin(process.argv)).argv
const numList = ['1','2','3','4','5','6','7','8','9','0'];
const Parser = require('expr-eval').Parser;
const errorTypes = {
	undefinedLbl: "Label '{}' is undefined",
	undefined: "Variable '{}' is undefined",
	unfinishedLbl: "Unfinished label '{}'",
	unfinishedStr: "Unfinished string '{}'",
	invalidEscape: "Invalid escape character \\{}",
	noInput: "No input file specified"
}

function getTokens(data){
  let toks = [];
  let tok = '';
  let mode = 0;
  data=data.split('');
  for(let i=0;i<data.length;i++){
    let c = data[i];
    if(mode==0){
      if(c=='\n'||c=='\r'){}
      if(c==' '||c=='\t'||c=='\n'||c=='\r'){
        switch(tok.toLowerCase()){
          case '':
            continue;
            break;
          case 'true':
            toks.push('BOL:TRUE');
            break;
          case 'false':
            toks.push('BOL:FALSE');
            break;
          default:
            toks.push('VAR:'+tok);
        }
        tok='';
        continue;
      }
      if(c=='['){
        tok='';
        mode=1;
        continue;
      }
      if((c in numList||c=='(')&&tok==''){
        tok=c;
        mode=3;
        continue;
      }
      if(c=='"'){
        if(tok!=''){
          
        }else{
          mode=2;
          continue;
        }
      }
      if(c=="'"){
        mode=2.5;
        continue;
      }
			if(c=='#'){
				mode=4;
				continue;
			}
      if(c=='='){
        try{
          if(toks[toks.length-1].substring(0,4)=='VAR:'&&tok==''){
            tok=toks[toks.length-1].substring(4);
            toks.pop();
          }
        }catch(err){}
        toks.push('SET!');
        toks.push('VAR:'+tok);
        tok='';
        continue;
      }
      tok=tok+c;
    }
    if(mode==1){
      if(c==']'){
        toks.push('LBL:'+tok);
        mode=0;
        tok='';
        continue;
      }
			if(c=='\n'||c=='\r'){
				logError('unfinishedLbl',tok);
			}
      tok=tok+c;
    }
    if(mode==2){
			if(tok.substring(tok.length-1)=='\\'){
				tok=tok.substring(0,tok.length-1);
				let x;
				switch(c){
					case '"':
						x=c;
						break;
					case "'":
						x=c;
						break;
					case 'n':
						x='\n';
						break;
					case 'r':
						x='\r';
						break;
					case 't':
						x='\t';
						break;
					default:
						logError('invalidEscape',c);
				}
				tok=tok+x;
				continue;
			}
      if(c=='"'){
        toks.push('STR:'+tok);
        tok='';
        mode=0;
        continue;
      }
			if(c=='\n'||c=='\r'){
				logError('unfinishedStr',tok);
			}
      tok=tok+c;
    }
    if(mode==2.5){
			if(tok.substring(tok.length-1)=='\\'){
				tok=tok.substring(0,tok.length-1);
				let x;
				switch(c){
					case '"':
						x=c;
						break;
					case "'":
						x=c;
						break;
					case 'n':
						x='\n';
						break;
					case 'r':
						x='\r';
						break;
					case 't':
						x='\t';
						break;
					default:
						logError('invalidEscape',c);
				}
				tok=tok+x;
				continue;
			}
      if(c=="'"){
        toks.push('STR:'+tok);
        tok='';
        mode=0;
        continue;
      }
			if(c=='\n'||c=='\r'){
				logError('unfinishedStr',tok);
			}
      tok=tok+c;
    }
    if(mode==3){
      if(c==' '||c=='\n'||c=='\t'||c=='\r'){
        toks.push('NUM:'+tok)
        tok='';
        mode=0;
        continue;
      }
      tok=tok+c;
    }
		if(mode==4){
			if(c=='\n'||c=='\r'){
				mode=0;
				continue;
			}
		}
  }
  return toks;
}

function parse(toks){
  let model={'main':{}};
  let curr='main';
  let skip=1;
  let getVars=function(lbl,v,flag){
    let keys = Object.keys(lbl);
    for(let i=0;i<keys.length;i++){
      let k = keys[i];
      if(k.substring(0,1) in numList){
        continue;
      }
      v[k]=lbl[k];
    }
    if(flag!=true){
      getVars(model.main,v,true);
    }
    return v;
  }
  let setVar=function(lbl,curr,key,val){
    let dt=val.substring(0,3);
    let content=val.substring(4);
    let nv=0;
    let vars=getVars(lbl,{});
    switch(dt){
      case 'STR':
        nv=content;
        break;
      case 'NUM':
        nv=Parser.evaluate(content,vars);
        break;
      case 'TRU':
        nv=true;
        break;
      case 'FLS':
        nv=false;
        break;
			case 'VAR':
				let vstruct = content.split('\.');
				if(vstruct.length>1){
					let lblNameToGet = vstruct[0];
					let varNameToGet = vstruct[1];
					let lblToGet = model[lblNameToGet];
					if(lblToGet===null||lblToGet===undefined){
						logError('undefinedLbl',lblNameToGet);
					}
					nv=lblToGet[varNameToGet];
					if(nv===undefined){
						logError('undefined',varNameToGet);
					}
				}else{
					let varNameToGet = vstruct[0];
					nv=lbl[varNameToGet]
					if(nv===undefined){
						let lblToGet = model['main'];
						nv=lblToGet[varNameToGet];
						if(nv===undefined){
							logError('undefined',varNameToGet);
						}
					}
				}
				break;
			case 'LBL':
				nv='@'+content;
				break;
      default:
        nv=null;
    }
    lbl[key]=nv;
    return nv;
  }
  let len=0;
  for(let i=0;i<toks.length;i+=skip){
    skip=1;
    let tok=toks[i];
    let thislabel = model[curr];
    let datatype=tok.substring(0,3);
    let value=tok.substring(4);
    switch(datatype){
      case 'SET':
        skip=3;
        let key=toks[i+1].substring(4);
        let val=toks[i+2];
        setVar(thislabel,curr,key,val);
        break;
      case 'LBL':
        curr=value;
        len=0;
        model[curr]={};
        break;
      default:
        setVar(thislabel,curr,len,tok);
        len++;
        break;
    }
  }
  if(Object.keys(model.main)==0){delete model.main;}
  return model;
}

function logError(type,arg){
	arg=arg||'';
	console.log('Error: '+errorTypes[type].split('{}').join(arg)+';');
	process.exit(1);
}

function logSeparator(text){
	text=text||'';
	let line = '\n---------------\n';
	console.log(line+text+line);
}

function main(file,ofile,verbose){
	ofile=ofile||'output.json';
	verbose=verbose||false;
  fs.readFile(file,(err,data)=>{
    let tokens = getTokens(''+data+'\n');
    let tree = parse(tokens);
		let json = JSON.stringify(tree);
		if(verbose===true){
			//console.clear();
			logSeparator('Tokens');
			console.log(tokens);
    	logSeparator('Tree');
			console.log(tree);
			logSeparator('JSON');
			console.log(json);
			console.log('\n');
		}
		fs.writeFile(ofile,json,()=>{
			console.log('Writing complete!');
		})
		console.log('writing JSON to file \''+ofile+'\';');
  });
}

let infile = argv.input||argv.i||argv._[0];
if(infile===undefined||infile===null){logError('noInput');}
let ofile = argv.output||argv.o||argv._[1]||'output.json';
let verbose = argv.verbose||argv.v||false;

main(infile,ofile,verbose);
