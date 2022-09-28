#ifndef _ENV_MINIMAX_TEST_H
#define _ENV_MINIMAX_TEST_H

#define RVTEST_RV32U
#define TESTNUM x28

#define RVTEST_CODE_BEGIN

#define RVTEST_PASS			\
.option rvc;				\
	c.li x8, -4;			\
	c.li x9, 0;			\
	c.sw x9, 0(x8);			\

#define RVTEST_FAIL			\
.option rvc;				\
	c.li x8, -4;			\
	c.li x9, 1;			\
	c.sw x9, 0(x8);			\

#define RVTEST_CODE_END
#define RVTEST_DATA_BEGIN .balign 4;
#define RVTEST_DATA_END

#endif
