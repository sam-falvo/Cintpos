#include <stdio.h>

int freleq(float x, float y) {
  return x==y;
    }

int frelne(float x, float y) {
  return x!=y;
    }

int frells(float x, float y) {
  return x<y;
    }

int frelgr(float x, float y) {
  return x>y;
    }

int frelle(float x, float y) {
  return x<=y;
    }

int frelge(float x, float y) {
  return x>=y;
    }


int freleq0(float x) {
  return x==0.0;
    }

int frelne0(float x) {
  return x!=0.0;
    }

int frells0(float x) {
  return x<0.0;
    }

int frelgr0(float x) {
  return x>0.0;
    }

int frelle0(float x) {
  return x<=0.0;
    }

int frelge0(float x) {
  return x>=0.0;
    }


void fjmpeq(float x, float y) {
  if(x==y) printf("true\n");
    }

void fjmpne(float x, float y) {
  if(x!=y) printf("true\n");
    }

void fjmpls(float x, float y) {
  if(x<y) printf("true\n");
    }

void fjmpgr(float x, float y) {
  if(x>y) printf("true\n");
    }

void fjmple(float x, float y) {
  if(x<=y) printf("true\n");
    }

void fjmpge(float x, float y) {
  if(x>=y) printf("true\n");
    }


void fjmpeq0(float x) {
  printf("%5.1f == 0.0 is ", x);
  if(x==0.0) printf("true\n");
  else  printf("false\n");
}

void fjmpne0(float x) {
  printf("%5.1f != 0.0 is ", x);
  if(x!=0.0) printf("true\n");
  else  printf("false\n");
}

void fjmpls0(float x) {
  printf("%5.1f <  0.0 is ", x);
  if(x<0.0) printf("true\n");
  else  printf("false\n");
}

void fjmpgr0(float x) {
  printf("%5.1f >  0.0 is ", x);
  if(x>0.0) printf("true\n");
  else  printf("false\n");
}

void fjmple0(float x) {
  printf("%5.1f <= 0.0 is ", x);
  if(x<=0.0) printf("true\n");
  else  printf("false\n");
}

void fjmpge0(float x) {
  printf("%5.1f >= 0.0 is ", x);
  if(x>=0.0) printf("true\n");
  else  printf("false\n");
}


int main() {

  printf("-1.2==0.7 => %d  0.7==0.7 => %d  1.2==0.7 => %d\n",
	  freleq(-1.2, 0.7), freleq(0.7, 0.7), freleq(1.2, 0.7));
  printf("-1.2!=0.7 => %d  0.7!=0.7 => %d  1.2!=0.7 => %d\n",
	  frelne(-1.2, 0.7), frelne(0.7, 0.7), frelne(1.2, 0.7));
  printf("-1.2< 0.7 => %d  0.7< 0.7 => %d  1.2< 0.7 => %d\n",
	  frells(-1.2, 0.7), frells(0.7, 0.7), frells(1.2, 0.7));
  printf("-1.2> 0.7 => %d  0.7> 0.7 => %d  1.2> 0.7 => %d\n",
	  frelgr(-1.2, 0.7), frelgr(0.7, 0.7), frelgr(1.2, 0.7));
  printf("-1.2<=0.7 => %d  0.7<=0.7 => %d  1.2<=0.7 => %d\n",
	  frelle(-1.2, 0.7), frelle(0.7, 0.7), frelle(1.2, 0.7));
  printf("-1.2>=0.7 => %d  0.7>=0.7 => %d  1.2>=0.7 => %d\n",
	  frelge(-1.2, 0.7), frelge(0.7, 0.7), frelge(1.2, 0.7));

  printf("\n");

  fjmpeq0(-1.2); fjmpeq0(0.0); fjmpeq0(1.2);
  fjmpne0(-1.2); fjmpne0(0.0); fjmpne0(1.2);
  fjmpls0(-1.2); fjmpls0(0.0); fjmpls0(1.2);
  fjmpgr0(-1.2); fjmpgr0(0.0); fjmpgr0(1.2);
  fjmple0(-1.2); fjmple0(0.0); fjmple0(1.2);
  fjmpge0(-1.2); fjmpge0(0.0); fjmpge0(1.2);
  return 0;
}
