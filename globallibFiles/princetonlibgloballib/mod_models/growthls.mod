#
#**************************
# SET UP THE INITIAL DATA *
#**************************
#   Problem :
#   *********
#   GROWTH problem in 3 variables
#   Fit the observed growth g(n) from Gaussian Elimination
#   with complete pivoting to a function of the form
#
#        U1 * n ** ( U2 + LOG(n) * U3 )
#   SIF input: Nick Gould, Nov, 1991, modified by Ph. Toint, March 1994.
#   classification SUR2-AN-3-0
#   Solution
	param n := 3;

	var u1 := 10.0;
	var u2;
	var u3;

#reformulate original objective to eliminate powers
#use b^x = exp(x * ln(b))

#minimize obj:
#	(u1 * (8.0^(u2+(log(8.0))*u3)) - 8.0)*(u1 * (8.0^(u2+(log(8.0))*u3)) - 8.0) + 
#	(u1 * (9.0^(u2+(log(9.0))*u3)) - 8.4305)*(u1 * (9.0^(u2+(log(9.0))*u3)) - 
#	8.4305) + (u1 * (10.0^(u2+2.302585093*u3)) - 9.5294)*(u1 * 
#	(10.0^(u2+2.302585093*u3)) - 9.5294) + (u1 * (11.0^(u2+2.397895273*u3)) - 
#	10.4627)*(u1 * (11.0^(u2+2.397895273*u3)) - 10.4627) + (u1 * 
#	(12.0^(u2+2.48490665*u3)) - 12.0)*(u1 * (12.0^(u2+2.48490665*u3)) - 12.0) + 
#	(u1 * (13.0^(u2+2.564949357*u3)) - 13.0205)*(u1 * (13.0^(u2+2.564949357*u3)) - 
#	13.0205) + (u1 * (14.0^(u2+2.63905733*u3)) - 14.5949)*(u1 * 
#	(14.0^(u2+2.63905733*u3)) - 14.5949) + (u1 * (15.0^(u2+2.708050201*u3)) - 
#	16.1078)*(u1 * (15.0^(u2+2.708050201*u3)) - 16.1078) + (u1 * 
#	(16.0^(u2+2.77258872223978*u3)) - 18.0596)*(u1 * (16.0^(u2+2.77258872223978*u3)) - 
#	18.0596) + (u1 * (18.0^(u2+2.890371758*u3)) - 20.4569)*(u1 * 
#	(18.0^(u2+2.890371758*u3)) - 20.4569) + (u1 * (20.0^(u2+2.99573227355399*u3)) - 
#	24.25)*(u1 * (20.0^(u2+2.99573227355399*u3)) - 24.25) + (u1 * 
#	(25.0^(u2+3.218875825*u3)) - 32.9863)*(u1 * (25.0^(u2+3.218875825*u3)) - 
#	32.9863);
	
minimize obj:
	(u1 * (exp( 2.079442*(u2+2.079441542*u3) )) - 8.0)*
	(u1 * (exp( 2.079442*(u2+2.079441542*u3) )) - 8.0) + 
	(u1 * (exp( 2.19722457733622*(u2+2.19722457733622*u3) )) - 8.4305)*
	(u1 * (exp( 2.19722457733622*(u2+2.19722457733622*u3) )) - 8.4305) + 
	(u1 * (exp( 2.302585*(u2+2.302585093*u3) )) - 9.5294)*
	(u1 * (exp( 2.302585*(u2+2.302585093*u3) )) - 9.5294) + 
	(u1 * (exp( 2.397895*(u2+2.397895273*u3) )) - 10.4627)*
	(u1 * (exp( 2.397895*(u2+2.397895273*u3) )) - 10.4627) + 
	(u1 * (exp( 2.484907*(u2+2.48490665*u3) )) - 12.0)*
	(u1 * (exp( 2.484907*(u2+2.48490665*u3) )) - 12.0) + 
	(u1 * (exp( 2.564949*(u2+2.564949357*u3) )) - 13.0205)*
	(u1 * (exp( 2.564949*(u2+2.564949357*u3) )) - 13.0205) + 
	(u1 * (exp( 2.639057*(u2+2.63905733*u3) )) - 14.5949)*
	(u1 * (exp( 2.639057*(u2+2.63905733*u3) )) - 14.5949) + 
	(u1 * (exp( 2.708050*(u2+2.708050201*u3) )) - 16.1078)*
	(u1 * (exp( 2.708050*(u2+2.708050201*u3) )) - 16.1078) + 
	(u1 * (exp( 2.772589*(u2+2.77258872223978*u3) )) - 18.0596)*
	(u1 * (exp( 2.772589*(u2+2.77258872223978*u3) )) - 18.0596) + 
	(u1 * (exp( 2.890372*(u2+2.890371758*u3) )) - 20.4569)*
	(u1 * (exp( 2.890372*(u2+2.890371758*u3) )) - 20.4569) + 
	(u1 * (exp( 2.995732*(u2+2.99573227355399*u3) )) - 24.25)*
	(u1 * (exp( 2.995732*(u2+2.99573227355399*u3) )) - 24.25) + 
	(u1 * (exp( 3.218876*(u2+3.218875825*u3) )) - 32.9863)*
	(u1 * (exp( 3.218876*(u2+3.218875825*u3) )) - 32.9863);

#edited by Luis M Rios luisrios@uiuc.edu
#imposed bound constraints on variables
s.t. Box1:
     -15 <= u2 <= 15;
s.t. Box2:
     -15 <= u3 <= 15;  
