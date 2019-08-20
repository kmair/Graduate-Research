#!/bin/sh -x
############################################
#
##########################################
#dir_models is the directory where the .mod models are located
dir_models=test
#dir_current is the current directory
dir_current=`pwd`
dir_exec=exec

#Y is the list of models
		for Y in \
		3pk\
		aircrftb\
		allinit\
		allinitu\
		arglinb\
		arglinc\
		beale\
		biggs3\
		biggs5\
		biggs6\
		box2\
		box3\
		bqp1var\
		branin\
		brkmcc\
		brownal\
		brownbs\
		brownden\
		camel1\
		camel6\
		chi\
		cliff\
		cube\
		denschna\
		denschnb\
		denschnc\
		denschnd\
		denschne\
		denschnf\
		dixon3dq\
		djtl\
		eg1\
		engval2\
		esfl\
		expfit\
		extrosnb\
		fermat2_vareps\
		fermat_vareps\
		genhumps\
		gold\
		griewank\
		growthls\
		growth\
		hairy\
		hart6\
		hatflda\
		hatfldb\
		hatfldc\
		hatfldd\
		heart6ls\
		heart8ls\
		hilberta\
		himmelbb\
		himmelbf\
		himmelbg\
		himmelp1\
		hs001\
		hs002\
		hs003\
		hs004\
		hs005\
		hs038\
		hs045\
		hs105\
		hs110\
		hs3mod\
		hs5\
		humps\
		jensmp\
		kowalik\
		kowosb\
		levy3\
		loghairy\
		logros\
		maratosb\
		maxlika\
		mdhole\
		median\
		median_vareps\
		mexhat\
		nasty\
		nonmsqrt\
		osborne1\
		osbornea\
		osborneb\
		oslbqp\
		palmer1c\
		palmer1d\
		palmer1e\
		palmer1\
		palmer2a\
		palmer2c\
		palmer2e\
		palmer3c\
		palmer3e\
		palmer3\
		palmer4c\
		palmer4e\
		palmer4\
		palmer5a\
		palmer5b\
		palmer5c\
		palmer5d\
		palmer5e\
		palmer6a\
		palmer6c\
		palmer6e\
		palmer7c\
		palmer7e\
		palmer8a\
		palmer8c\
		palmer8e\
		polygon\
		powell\
		price\
		pspdoc\
		qudlin\
		rosenbr\
		s201\
		s202\
		s204\
		s205\
		s206\
		s207\
		s208\
		s209\
		s210\
		s211\
		s212\
		s213\
		s214\
		s229\
		s240\
		s242\
		s243\
		s244\
		s245\
		s246\
		s256\
		s257\
		s258\
		s259\
		s260\
		s261\
		s266\
		s267\
		s271\
		s272\
		s273\
		s274\
		s275\
		s276\
		s281\
		s282\
		s283\
		s286\
		s287\
		s288\
		s289\
		s290\
		s291\
		s292\
		s294\
		s295\
		s296\
		s297\
		s300\
		s303\
		s308\
		s309\
		s311\
		s312\
		s314\
		s328\
		s333\
		s350\
		s351\
		s352\
		s358\
		s368\
		s370\
		s371\
		s379\
		s386\
		schwefel\
		shekel\
		sim2bqp\
		simbqp\
		sineali\
		sineval\
		sisser\
		steiner_vareps\
		tre\
		yfit\
		yfitu\
		zangwil2\
		ex4_1_1\
		ex4_1_2\
		ex4_1_3\
		ex4_1_4\
		ex4_1_5\
		ex4_1_6\
		ex4_1_7\
		ex8_1_1\
		ex8_1_2\
		ex8_1_3\
		ex8_1_4\
		ex8_1_5\
		ex8_1_6\
		least\
		rbrock\
		st_bsj3\
		st_cqpjk2\
		st_e39
		do

		echo $X
		echo "model ${Y}.mod;" > $Y.run
		echo option solver '"'modtocode'";' >> $Y.run
		echo "solve;" >> $Y.run

		echo $Y > problem_name
		ampl $Y.run > $Y.results
		rm -f $Y.results
		mv input.in $Y.input.in
		mv problem.data $Y.problem.data
		mv amplmodel_code.c $Y.c
		
		done
		#models

#clean intermediate files
rm *.run
