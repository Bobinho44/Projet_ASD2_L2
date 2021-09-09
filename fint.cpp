#include "fint.h"


/*
	constructeur à partir d'un entier n >= 0
*/
fint::fint(int_t n) {
	map<int, int> c;
	// Gestion des cas particuliers (0 et 1)
	if (n == 0) {c[0] = 1;}
	else if (n == 1) {c[1] = 1;}

	else if (n > 1) {

		// Recherche de multiple de n
		while (n != 1) {
			int i;
			for (i = 2; n % i != 0; i++) {}
			c[i]++;
			n /= i;
		}
	setFactorsMap(c);
	} 
	else {
		throw string("n doit être supérieur ou égall à 0.");
	}
}


/*
	constructeur à partir d'une liste de facteurs premiers lf
	et d'une liste de multiplicité lm telles que lm et lf ont la
	même taille et lm[i]>0 est la multiplicité de lf[i] pour tout i
	exemple d'appel de ce constructeur :
	fint f({2,5,11}, {1,2,2});
*/
fint::fint(const initializer_list<int_t>& lf, const initializer_list<mult_t>& lm) {
	if (lf.size() == lm.size()) {
		map<int, int> c;

		// Gestion des cas particuliers (0 et 1)
		if (*lf.begin() == 0 && *lm.begin() == 1) {c[0] = 1; return;}
		if (*lf.begin() == 1 && *lm.begin() == 1) {c[1] = 1; return;}

		// Vérification de la primalité des facteurs et de la positivité des multiplicités
		for (auto fact = lf.begin(), mult = lm.begin(); fact != lf.end(); ++fact, ++mult) {
			if (*mult > 0) {
				for (int j = 2; j <= sqrt(*fact) || *fact < 2; j++) {
					if ((*fact % j == 0 && j != *fact) || *fact < 2) {
						throw string("Les facteurs doivent être premier.");
					}
				}

				// insertion des couples (lf[i], lm[i]) dans le dictionnaire
				c[*fact] += *mult;
			}
			else {
				throw string("Les multiplicités doivent être > 0.");
			}
		}
		setFactorsMap(c);
	}
	else {
		throw string("La taille des listes doivent être identique.");
	}
}


/*
	destructeur
*/
fint::~fint() {
	getFactorsMap().clear();
}


/*
	retourne la valeur décimale de this, throws std::overflow_error
*/
int_t fint::to_int() const {
	int_t entier = 1;
	for ( const auto &p : getFactorsMap() ) {
		entier *= pow(p.first, p.second);
	}
	if (entier <= MAX_INT_T) {
		return entier;
	}
	else {
		throw overflow_error("Ce nombre est trop grand.");
	}
}


/*
	teste si this divise a
*/
bool fint::divides(const fint& a) const {

	// Si this à plus de facteurs que a, alors il possède au moins un facteur que a n’a pas
	if (getFactorsMap().size() > a.getFactorsMap().size()) {return false;}

	for ( const auto &p : getFactorsMap() ) {

		// Si this possède un facteur que a n’a pas ou si pour un facteur, 
		if (a.getFactorsMap().count(p.first) == 0) {return false;}

		// Si la multiplicité associée dans this est supérieur à celle dans a
		else if (a.getFactorsMap().at(p.first) - p.second < 0) {return false;}
	}
	return true;
}


/*
	teste si this est premier
*/
bool fint::is_prime() const {

	// Si le fint ne représente pas 1 ou 0 et qu’il est composé que 
	// d’un facteur avec une multiplicité 1, alors il est premier
	return (getFactorsMap().size() == 1 && getFactorsMap().begin()->first != 0 && getFactorsMap().begin()->first != 1 && getFactorsMap().begin()->second == 1);
}


/*
	comparaisons
*/
bool operator==(const fint& a, const fint& b) {

	// Si les deux fints ne sont pas totalement identique (même couple (facteur, multiplicité))
	return a.getFactorsMap() == b.getFactorsMap();
}


/*
	comparaisons
*/
bool operator!=(const fint& a, const fint& b) {
	return a.getFactorsMap() != b.getFactorsMap();
}


/*
	retourne le plus petit commun multiple de a et b
*/
fint lcm(const fint& a, const fint& b) {
	fint c = a*b;
	map<int, int> cMap = c.getFactorsMap();

	// Suppression de l’intersection entre a et b de leurs multiplications
	for (const auto &p: a.getFactorsMap()) {
		if (b.getFactorsMap().count(p.first)) {
			cMap[p.first] -= min(p.second, b.getFactorsMap().at(p.first));
		}		
	}
	c.setFactorsMap(cMap);
	return c;
}


/*
	retourne le plus grand diviseur commun de a et b
*/
fint gcd(const fint& a, const fint& b) {
	fint c(1);
	map<int, int> cMap = c.getFactorsMap();

	// Intersection des deux fints
    for (const auto &p: a.getFactorsMap()) {
        if (b.getFactorsMap().find(p.first) != b.getFactorsMap().end()) {
			cMap[p.first] = min(p.second, b.getFactorsMap().at(p.first));
		}
    }
	if (cMap.size() != 1) cMap.erase(1);
	c.setFactorsMap(cMap);
	return c;
}


/*
	retourne le numérateur (a) et le dénominateur (b) de la fraction a/b 
	sous forme réduite
*/
vector<fint> reduce(const fint& a, const fint& b) {
	fint c = gcd(a,b);
	return {a/c, b/c};
}


/*
	retourne a * b
*/
fint operator*(const fint& a, const fint& b) {

	// Si un des fints représente 0
	if (a.getFactorsMap().count(0) == 1 || b.getFactorsMap().count(0) == 1) {return fint (0);}
	
	fint c = a;
	map<int, int> cMap = c.getFactorsMap();
	 // Union des deux fint et accumulation des multiplicités en cas de facteurs communs
    for ( const auto &p : b.getFactorsMap() ) {
		cMap[p.first] += p.second;
	}

	// Si un des fints représente 1
	if (cMap.size() != 1) cMap.erase(1);
	c.setFactorsMap(cMap);
	return c;
}


/*
	retourne a / b si b divise a, throws std::domain_error sinon
*/
fint operator/(const fint& a, const fint& b) {
	fint c = a;
	map<int, int> cMap = c.getFactorsMap();
	if (b.divides(a)) {

		// Diminution des multiplicité de a de –(multiplicité de b) en cas de facteurs communs
		for ( const auto &p : b.getFactorsMap() ) {
			cMap[p.first] -= p.second;

			// Suppression des facteurs ayant des multiplicités nulles
			if (cMap[p.first] == 0) {cMap.erase(p.first);}
		}
	} 
	else {
		throw domain_error("La division est impossible");
	}
	c.setFactorsMap(cMap);
	return c;
}


/*
	retourne a % b
*/
fint operator%(const fint& a, const fint& b) {
	int_t modulo = 1;
	int_t entier = b.to_int();
	for ( const auto &p : a.getFactorsMap() ) {

		// Utilisation d’une propriété de distributivité de la congruence
		modulo *=  (int_t) pow(p.first, p.second) % entier;
	}
	return fint (modulo % entier);
}


/*
	retourne a puissance n
*/
fint pow(const fint& a, unsigned int n) {

	// Gestion des cas particuliers (0 et 1)
	if (a.getFactorsMap().count(0) == 1) {return fint (0);}
	if (a.getFactorsMap().count(1) == 1) {return fint (1);}

	fint c = a;	
	map<int, int> cMap = c.getFactorsMap();
	for ( const auto &p : a.getFactorsMap() ) {

		 // Multiplication des multiplicités de a par n
		cMap[p.first] *= n;
	}
	c.setFactorsMap(cMap);
	return c;
}


/*
	écriture de a sur un flot de sortie
*/
ostream& operator<<(ostream& os, const fint& a) {
	for ( const auto &p : a.getFactorsMap() ) {
		os << (p.first == a.getFactorsMap().begin()->first ? "" : " × ") << p.first << "^" << p.second;
	}
	os << endl;
	return os;
}