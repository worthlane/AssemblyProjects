#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

static const char* FILE_NAME = "input.txt";

int main()
{
    FILE* fp = fopen(FILE_NAME, "w");
    assert(fp);

    /*for (int i = 0; i < 679; i++)
    {
        fputc(1, fp);
    }*/

    for (int i = 0; i < 64853; i++)
    {
        fputc('x', fp);
    }

    fputc('!', fp);
    fputc(0, fp);

    fputc(13, fp);
    fputc(13, fp);
    fputc(13, fp);
    fputc(13, fp);

    fclose(fp);

    return 0;
}
