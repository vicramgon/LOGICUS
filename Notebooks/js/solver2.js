function parseClauses(cs, vars) {
    
    function parseVars(v, vars) {
        let ret = Array(length(vars)).fill(null);
        for (let variable of v.split(",")){
            if (variable.charAt(0)=="¬"){
                ret[vars.indexOf(variable.substring(1))] = 0;
            } else {
                ret[vars.indexOf(variable)] = 1;
            }   
        }
        return ret;
    }

    return cs.split(';').map((x) => parseVars(x, vars));
    
}

function deepcopy(o) {
    return JSON.parse(JSON.stringify(o));
}

function length(o) {
    return Object.keys(o).length;
}

function isEmpty(o){
    return Object.keys(o).length == 0;
}




function satSolver(clauses_in){

    function cleanClauses (clauses, best_var, value){
        res=[]
        for (let c of clauses){
            if (c[best_var] !== value){
                c_c = c;
                c_c[best_var] = null;
                res.push(c_c);
            }
        }
        return res;
    }

    function satSolverAux(asig, av_vars, av_clauses){
        if (isEmpty(av_clauses)){
            return asig;
        } else if (av_clauses.some((x) => av_clauses.every((y) => y === null))){
            return null;
        }else{
            let best_var = null;
            let best_var_v = null;

            for (let c of av_clauses){
                let c_aux = [...c.keys()].filter((x) => c[x]!== null);
                if (c_aux.length === 1){
                    best_var = c_aux[0];
                    best_var_v =[c[best_var]];
                    break;
                }
            }

            

            if (best_var == null){
                let av_vars_f = av_vars.filter((x) => x !== null);
                best_var = av_vars_f[Math.floor(Math.random()*av_vars_f.length)];
                best_var_v = Math.random() > 0.5?[1,0]:[0,1];
            }

            av_vars[best_var]=null;

            for (let value of best_var_v){
                asig[best_var]=value;
                let aux = cleanClauses (av_clauses, best_var, value);
                let solution = satSolverAux(asig,  av_vars, aux);
                if (solution !== null){
                    return solution;
                }
            }
            av_vars[best_var]=best_var;
            delete asig[best_var];
        }
        return null;
    }
    
    var clauses_in_aux = clauses_in.split("#");
    var vars = clauses_in_aux[0].split(",");
    var clauses = parseClauses(clauses_in_aux[1], vars);
    var solved = satSolverAux([], [...Array(length(vars)).fill(null).keys()], clauses);
    if (solved !== null){
        var result = "Satisfactible: true variables are [  ";
        for (v in solved){
            if (solved[v] == 1){
                result += vars[v] + ", ";
            }

        }
        result = result.substring(0, result.length-2);
        result += "  ]";



        return result;
    } else {
        return "This problem isn't satisfactible";
    }
}



console.log(satSolver("p00,p01,p02,p03,p04,p05,p06,p07,p10,p11,p12,p13,p14,p15,p16,p17,p20,p21,p22,p23,p24,p25,p26,p27,p30,p31,p32,p33,p34,p35,p36,p37,p40,p41,p42,p43,p44,p45,p46,p47,p50,p51,p52,p53,p54,p55,p56,p57,p60,p61,p62,p63,p64,p65,p66,p67,p70,p71,p72,p73,p74,p75,p76,p77#p11,p13,p15,p17,p16,p14,p12,p10;p31,p33,p35,p37,p36,p34,p32,p30;p51,p53,p55,p57,p56,p54,p52,p50;p71,p73,p75,p77,p76,p74,p72,p70;p61,p63,p65,p67,p66,p64,p62,p60;p41,p43,p45,p47,p46,p44,p42,p40;p21,p23,p25,p27,p26,p24,p22,p20;p01,p03,p05,p07,p06,p04,p02,p00;¬p01,¬p11;¬p03,¬p13;¬p05,¬p15;¬p07,¬p17;¬p11,¬p21;¬p01,¬p21;¬p13,¬p23;¬p03,¬p23;¬p15,¬p25;¬p05,¬p25;¬p17,¬p27;¬p07,¬p27;¬p11,¬p31;¬p21,¬p31;¬p01,¬p31;¬p13,¬p33;¬p23,¬p33;¬p03,¬p33;¬p15,¬p35;¬p25,¬p35;¬p05,¬p35;¬p17,¬p37;¬p27,¬p37;¬p07,¬p37;¬p11,¬p41;¬p31,¬p41;¬p21,¬p41;¬p01,¬p41;¬p13,¬p43;¬p33,¬p43;¬p23,¬p43;¬p03,¬p43;¬p15,¬p45;¬p35,¬p45;¬p25,¬p45;¬p05,¬p45;¬p17,¬p47;¬p37,¬p47;¬p27,¬p47;¬p07,¬p47;¬p11,¬p51;¬p31,¬p51;¬p41,¬p51;¬p21,¬p51;¬p01,¬p51;¬p13,¬p53;¬p33,¬p53;¬p43,¬p53;¬p23,¬p53;¬p03,¬p53;¬p15,¬p55;¬p35,¬p55;¬p45,¬p55;¬p25,¬p55;¬p05,¬p55;¬p17,¬p57;¬p37,¬p57;¬p47,¬p57;¬p27,¬p57;¬p07,¬p57;¬p11,¬p61;¬p31,¬p61;¬p51,¬p61;¬p41,¬p61;¬p21,¬p61;¬p01,¬p61;¬p13,¬p63;¬p33,¬p63;¬p53,¬p63;¬p43,¬p63;¬p23,¬p63;¬p03,¬p63;¬p15,¬p65;¬p35,¬p65;¬p55,¬p65;¬p45,¬p65;¬p25,¬p65;¬p05,¬p65;¬p17,¬p67;¬p37,¬p67;¬p57,¬p67;¬p47,¬p67;¬p27,¬p67;¬p07,¬p67;¬p11,¬p71;¬p31,¬p71;¬p51,¬p71;¬p61,¬p71;¬p41,¬p71;¬p21,¬p71;¬p01,¬p71;¬p13,¬p73;¬p33,¬p73;¬p53,¬p73;¬p63,¬p73;¬p43,¬p73;¬p23,¬p73;¬p03,¬p73;¬p15,¬p75;¬p35,¬p75;¬p55,¬p75;¬p65,¬p75;¬p45,¬p75;¬p25,¬p75;¬p05,¬p75;¬p07,¬p77;¬p27,¬p77;¬p47,¬p77;¬p67,¬p77;¬p57,¬p77;¬p37,¬p77;¬p17,¬p77;¬p76,¬p66;¬p74,¬p64;¬p72,¬p62;¬p70,¬p60;¬p76,¬p56;¬p66,¬p56;¬p74,¬p54;¬p64,¬p54;¬p72,¬p52;¬p62,¬p52;¬p70,¬p50;¬p60,¬p50;¬p56,¬p46;¬p76,¬p46;¬p66,¬p46;¬p54,¬p44;¬p74,¬p44;¬p64,¬p44;¬p52,¬p42;¬p72,¬p42;¬p62,¬p42;¬p50,¬p40;¬p70,¬p40;¬p60,¬p40;¬p56,¬p36;¬p76,¬p36;¬p66,¬p36;¬p46,¬p36;¬p54,¬p34;¬p74,¬p34;¬p64,¬p34;¬p44,¬p34;¬p52,¬p32;¬p72,¬p32;¬p62,¬p32;¬p42,¬p32;¬p50,¬p30;¬p70,¬p30;¬p60,¬p30;¬p40,¬p30;¬p36,¬p26;¬p56,¬p26;¬p76,¬p26;¬p66,¬p26;¬p46,¬p26;¬p34,¬p24;¬p54,¬p24;¬p74,¬p24;¬p64,¬p24;¬p44,¬p24;¬p32,¬p22;¬p52,¬p22;¬p72,¬p22;¬p62,¬p22;¬p42,¬p22;¬p30,¬p20;¬p50,¬p20;¬p70,¬p20;¬p60,¬p20;¬p40,¬p20;¬p36,¬p16;¬p56,¬p16;¬p76,¬p16;¬p66,¬p16;¬p46,¬p16;¬p26,¬p16;¬p34,¬p14;¬p54,¬p14;¬p74,¬p14;¬p64,¬p14;¬p44,¬p14;¬p24,¬p14;¬p32,¬p12;¬p52,¬p12;¬p72,¬p12;¬p62,¬p12;¬p42,¬p12;¬p22,¬p12;¬p30,¬p10;¬p50,¬p10;¬p70,¬p10;¬p60,¬p10;¬p40,¬p10;¬p20,¬p10;¬p16,¬p06;¬p36,¬p06;¬p56,¬p06;¬p76,¬p06;¬p66,¬p06;¬p46,¬p06;¬p26,¬p06;¬p14,¬p04;¬p34,¬p04;¬p54,¬p04;¬p74,¬p04;¬p64,¬p04;¬p44,¬p04;¬p24,¬p04;¬p12,¬p02;¬p32,¬p02;¬p52,¬p02;¬p72,¬p02;¬p62,¬p02;¬p42,¬p02;¬p22,¬p02;¬p10,¬p00;¬p30,¬p00;¬p50,¬p00;¬p70,¬p00;¬p60,¬p00;¬p40,¬p00;¬p20,¬p00;¬p01,¬p03;¬p01,¬p05;¬p03,¬p05;¬p01,¬p07;¬p03,¬p07;¬p05,¬p07;¬p11,¬p13;¬p11,¬p15;¬p13,¬p15;¬p11,¬p17;¬p13,¬p17;¬p15,¬p17;¬p21,¬p23;¬p21,¬p25;¬p23,¬p25;¬p21,¬p27;¬p23,¬p27;¬p25,¬p27;¬p31,¬p33;¬p31,¬p35;¬p33,¬p35;¬p31,¬p37;¬p33,¬p37;¬p35,¬p37;¬p41,¬p43;¬p41,¬p45;¬p43,¬p45;¬p41,¬p47;¬p43,¬p47;¬p45,¬p47;¬p51,¬p53;¬p51,¬p55;¬p53,¬p55;¬p51,¬p57;¬p53,¬p57;¬p55,¬p57;¬p61,¬p63;¬p61,¬p65;¬p63,¬p65;¬p61,¬p67;¬p63,¬p67;¬p65,¬p67;¬p71,¬p73;¬p71,¬p75;¬p73,¬p75;¬p75,¬p77;¬p73,¬p77;¬p71,¬p77;¬p77,¬p76;¬p75,¬p76;¬p73,¬p76;¬p71,¬p76;¬p75,¬p74;¬p77,¬p74;¬p76,¬p74;¬p73,¬p74;¬p71,¬p74;¬p73,¬p72;¬p75,¬p72;¬p77,¬p72;¬p76,¬p72;¬p74,¬p72;¬p71,¬p72;¬p71,¬p70;¬p73,¬p70;¬p75,¬p70;¬p77,¬p70;¬p76,¬p70;¬p74,¬p70;¬p72,¬p70;¬p67,¬p66;¬p65,¬p66;¬p63,¬p66;¬p61,¬p66;¬p65,¬p64;¬p67,¬p64;¬p66,¬p64;¬p63,¬p64;¬p61,¬p64;¬p63,¬p62;¬p65,¬p62;¬p67,¬p62;¬p66,¬p62;¬p64,¬p62;¬p61,¬p62;¬p61,¬p60;¬p63,¬p60;¬p65,¬p60;¬p67,¬p60;¬p66,¬p60;¬p64,¬p60;¬p62,¬p60;¬p57,¬p56;¬p55,¬p56;¬p53,¬p56;¬p51,¬p56;¬p55,¬p54;¬p57,¬p54;¬p56,¬p54;¬p53,¬p54;¬p51,¬p54;¬p53,¬p52;¬p55,¬p52;¬p57,¬p52;¬p56,¬p52;¬p54,¬p52;¬p51,¬p52;¬p51,¬p50;¬p53,¬p50;¬p55,¬p50;¬p57,¬p50;¬p56,¬p50;¬p54,¬p50;¬p52,¬p50;¬p47,¬p46;¬p45,¬p46;¬p43,¬p46;¬p41,¬p46;¬p45,¬p44;¬p47,¬p44;¬p46,¬p44;¬p43,¬p44;¬p41,¬p44;¬p43,¬p42;¬p45,¬p42;¬p47,¬p42;¬p46,¬p42;¬p44,¬p42;¬p41,¬p42;¬p41,¬p40;¬p43,¬p40;¬p45,¬p40;¬p47,¬p40;¬p46,¬p40;¬p44,¬p40;¬p42,¬p40;¬p37,¬p36;¬p35,¬p36;¬p33,¬p36;¬p31,¬p36;¬p35,¬p34;¬p37,¬p34;¬p36,¬p34;¬p33,¬p34;¬p31,¬p34;¬p33,¬p32;¬p35,¬p32;¬p37,¬p32;¬p36,¬p32;¬p34,¬p32;¬p31,¬p32;¬p31,¬p30;¬p33,¬p30;¬p35,¬p30;¬p37,¬p30;¬p36,¬p30;¬p34,¬p30;¬p32,¬p30;¬p27,¬p26;¬p25,¬p26;¬p23,¬p26;¬p21,¬p26;¬p25,¬p24;¬p27,¬p24;¬p26,¬p24;¬p23,¬p24;¬p21,¬p24;¬p23,¬p22;¬p25,¬p22;¬p27,¬p22;¬p26,¬p22;¬p24,¬p22;¬p21,¬p22;¬p21,¬p20;¬p23,¬p20;¬p25,¬p20;¬p27,¬p20;¬p26,¬p20;¬p24,¬p20;¬p22,¬p20;¬p17,¬p16;¬p15,¬p16;¬p13,¬p16;¬p11,¬p16;¬p15,¬p14;¬p17,¬p14;¬p16,¬p14;¬p13,¬p14;¬p11,¬p14;¬p13,¬p12;¬p15,¬p12;¬p17,¬p12;¬p16,¬p12;¬p14,¬p12;¬p11,¬p12;¬p11,¬p10;¬p13,¬p10;¬p15,¬p10;¬p17,¬p10;¬p16,¬p10;¬p14,¬p10;¬p12,¬p10;¬p07,¬p06;¬p05,¬p06;¬p03,¬p06;¬p01,¬p06;¬p05,¬p04;¬p07,¬p04;¬p06,¬p04;¬p03,¬p04;¬p01,¬p04;¬p03,¬p02;¬p05,¬p02;¬p07,¬p02;¬p06,¬p02;¬p04,¬p02;¬p01,¬p02;¬p01,¬p00;¬p03,¬p00;¬p05,¬p00;¬p07,¬p00;¬p06,¬p00;¬p04,¬p00;¬p02,¬p00;¬p01,¬p12;¬p03,¬p14;¬p05,¬p16;¬p10,¬p32;¬p12,¬p34;¬p01,¬p34;¬p14,¬p36;¬p03,¬p36;¬p20,¬p42;¬p22,¬p44;¬p24,¬p46;¬p30,¬p52;¬p32,¬p54;¬p10,¬p54;¬p12,¬p56;¬p34,¬p56;¬p01,¬p56;¬p40,¬p62;¬p42,¬p64;¬p20,¬p64;¬p44,¬p66;¬p22,¬p66;¬p60,¬p71;¬p62,¬p73;¬p40,¬p73;¬p64,¬p75;¬p42,¬p75;¬p20,¬p75;¬p22,¬p77;¬p44,¬p77;¬p66,¬p77;¬p10,¬p76;¬p32,¬p76;¬p54,¬p76;¬p30,¬p74;¬p52,¬p74;¬p50,¬p72;¬p01,¬p67;¬p56,¬p67;¬p34,¬p67;¬p12,¬p67;¬p10,¬p65;¬p32,¬p65;¬p54,¬p65;¬p76,¬p65;¬p30,¬p63;¬p52,¬p63;¬p74,¬p63;¬p50,¬p61;¬p72,¬p61;¬p24,¬p57;¬p46,¬p57;¬p22,¬p55;¬p44,¬p55;¬p77,¬p55;¬p66,¬p55;¬p20,¬p53;¬p42,¬p53;¬p75,¬p53;¬p64,¬p53;¬p40,¬p51;¬p73,¬p51;¬p62,¬p51;¬p03,¬p47;¬p36,¬p47;¬p14,¬p47;¬p01,¬p45;¬p56,¬p45;¬p67,¬p45;¬p34,¬p45;¬p12,¬p45;¬p10,¬p43;¬p32,¬p43;¬p65,¬p43;¬p76,¬p43;¬p54,¬p43;¬p30,¬p41;¬p63,¬p41;¬p74,¬p41;¬p52,¬p41;¬p26,¬p37;¬p24,¬p35;¬p57,¬p35;¬p46,¬p35;¬p22,¬p33;¬p55,¬p33;¬p77,¬p33;¬p66,¬p33;¬p44,¬p33;¬p20,¬p31;¬p53,¬p31;¬p75,¬p31;¬p64,¬p31;¬p42,¬p31;¬p05,¬p27;¬p16,¬p27;¬p03,¬p25;¬p36,¬p25;¬p47,¬p25;¬p14,¬p25;¬p01,¬p23;¬p34,¬p23;¬p56,¬p23;¬p67,¬p23;¬p45,¬p23;¬p12,¬p23;¬p10,¬p21;¬p43,¬p21;¬p65,¬p21;¬p76,¬p21;¬p54,¬p21;¬p32,¬p21;¬p37,¬p15;¬p26,¬p15;¬p35,¬p13;¬p57,¬p13;¬p46,¬p13;¬p24,¬p13;¬p33,¬p11;¬p55,¬p11;¬p77,¬p11;¬p66,¬p11;¬p44,¬p11;¬p22,¬p11;¬p17,¬p06;¬p15,¬p04;¬p37,¬p04;¬p26,¬p04;¬p13,¬p02;¬p35,¬p02;¬p57,¬p02;¬p46,¬p02;¬p24,¬p02;¬p11,¬p00;¬p33,¬p00;¬p55,¬p00;¬p77,¬p00;¬p66,¬p00;¬p44,¬p00;¬p22,¬p00;¬p02,¬p20;¬p04,¬p22;¬p06,¬p24;¬p12,¬p30;¬p14,¬p32;¬p16,¬p34;¬p22,¬p40;¬p04,¬p40;¬p24,¬p42;¬p06,¬p42;¬p26,¬p44;¬p14,¬p50;¬p32,¬p50;¬p16,¬p52;¬p34,¬p52;¬p36,¬p54;¬p42,¬p60;¬p24,¬p60;¬p06,¬p60;¬p26,¬p62;¬p44,¬p62;¬p46,¬p64;¬p16,¬p70;¬p34,¬p70;¬p52,¬p70;¬p36,¬p72;¬p54,¬p72;¬p56,¬p74;¬p66,¬p75;¬p64,¬p73;¬p46,¬p73;¬p62,¬p71;¬p44,¬p71;¬p26,¬p71;¬p76,¬p67;¬p74,¬p65;¬p56,¬p65;¬p72,¬p63;¬p54,¬p63;¬p36,¬p63;¬p70,¬p61;¬p52,¬p61;¬p34,¬p61;¬p16,¬p61;¬p66,¬p57;¬p75,¬p57;¬p64,¬p55;¬p73,¬p55;¬p46,¬p55;¬p62,¬p53;¬p71,¬p53;¬p44,¬p53;¬p26,¬p53;¬p06,¬p51;¬p24,¬p51;¬p42,¬p51;¬p60,¬p51;¬p56,¬p47;¬p74,¬p47;¬p65,¬p47;¬p54,¬p45;¬p72,¬p45;¬p63,¬p45;¬p36,¬p45;¬p52,¬p43;¬p70,¬p43;¬p61,¬p43;¬p34,¬p43;¬p16,¬p43;¬p50,¬p41;¬p32,¬p41;¬p14,¬p41;¬p46,¬p37;¬p64,¬p37;¬p73,¬p37;¬p55,¬p37;¬p44,¬p35;¬p62,¬p35;¬p71,¬p35;¬p53,¬p35;¬p26,¬p35;¬p06,¬p33;¬p24,¬p33;¬p51,¬p33;¬p60,¬p33;¬p42,¬p33;¬p04,¬p31;¬p22,¬p31;¬p40,¬p31;¬p36,¬p27;¬p54,¬p27;¬p72,¬p27;¬p63,¬p27;¬p45,¬p27;¬p34,¬p25;¬p52,¬p25;¬p70,¬p25;¬p61,¬p25;¬p43,¬p25;¬p16,¬p25;¬p32,¬p23;¬p50,¬p23;¬p41,¬p23;¬p14,¬p23;¬p30,¬p21;¬p12,¬p21;¬p26,¬p17;¬p44,¬p17;¬p62,¬p17;¬p71,¬p17;¬p53,¬p17;¬p35,¬p17;¬p06,¬p15;¬p33,¬p15;¬p51,¬p15;¬p60,¬p15;¬p42,¬p15;¬p24,¬p15;¬p04,¬p13;¬p31,¬p13;¬p40,¬p13;¬p22,¬p13;¬p02,¬p11;¬p20,¬p11;¬p16,¬p07;¬p34,¬p07;¬p52,¬p07;¬p70,¬p07;¬p61,¬p07;¬p43,¬p07;¬p25,¬p07;¬p14,¬p05;¬p32,¬p05;¬p50,¬p05;¬p41,¬p05;¬p23,¬p05;¬p12,¬p03;¬p30,¬p03;¬p21,¬p03;¬p10,¬p01"));
