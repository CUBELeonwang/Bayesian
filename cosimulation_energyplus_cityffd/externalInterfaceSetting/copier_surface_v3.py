############ By I.K.H.O.T ############

from sys import argv

if len(argv)<5:
    print("[ERR] 过少参数\r\n用法：python copier_surface.py [IDF模板] [CSV数据表] [输出IDF] [修改位置定位字符串1] [(可选)修改位置定位字符串2] [(可选)修改位置定位字符串3]...\r\n注意：\r\n1.CSV数据表不可包含表头\r\n2.文件路径使用绝对路径\r\n3.命令参数不包括中括号\r\n")
    exit(1)
csvp="/Users/i.k.h.o.t/Downloads/surface_number.csv"
infp="/Users/i.k.h.o.t/Downloads/simplemodel_everyfloorisonethermal_addDDY_externalInterface_DH.idf"
oufp="/Users/i.k.h.o.t/Downloads/simplemodel_everyfloorisonethermal_addDDY_externalInterface_DH_output.idf"
pred="OutdoorAir:Node,"

try:
    inf=open(argv[1])
except:
    print("[ERR] 无法访问文件")
    exit(2)
try:
    inc=inf.readlines()
except:
    print("[ERR] 读取错误")
    exit(3)
for l in range(4,len(argv),1):
    try:
        lspl=False
        rspl=False
        mspl=False
        swi=False
        for i in range(0,len(inc),1):
            if inc[i].replace("\n","").replace("\r","")==argv[l]:
                lspl=inc[:i]
                mspl=i
            elif inc[i]=="\n" and mspl:
                if swi:
                    mspl=inc[mspl:i]
                    rspl=i
                    break
                else:
                    swi=False
                    lspl=False
                    mspl=False
            elif inc[i].find("%fillcsv%")>=0 and mspl:
                swi=True
        if lspl==False or mspl==False:
            print("[ERR] 输入格式错误")
            exit(3)
        if rspl==False:
            print("[ERR] 输入格式错误")
            exit(3)
        rspl=inc[rspl:]
    except:
        print("[ERR] 处理错误")
        exit(3)
    try:
        cvf=open(argv[2])
    except:
        print("[ERR] 无法访问文件")
        exit(2)
    try:
        cvc=cvf.readlines()
    except:
        print("[ERR] 读取错误")
        exit(3)
    cpy=[]
    for i in cvc:
        spl=i.split(",")
        try:
            cnv=int(spl[0])
        except:
            print("[ERR] CSV格式错误")
            exit(3)
        for k in mspl:
            if k.find("%fillcsv%")>=0:
                cpy.append(k.replace("%fillcsv%",str(int(spl[0]))))
            else:
                cpy.append(k)
        cpy.append("\n")
    try:
        ouf=open(argv[3],"w")
    except:
        print("[ERR] 无法访问文件")
        exit(1)
    try:
        ouf.writelines(lspl)
        ouf.writelines(cpy)
        ouf.writelines(rspl)
    except:
        print("[ERR] 写入错误")
        exit(2)
    try:
        ouf.close()
    except:
        print("[ERR] 操作错误")
        exit(3)
    try:
        inf=open(argv[3])
    except:
        print("[ERR] 无法访问文件")
        exit(2)
    try:
        inc=inf.readlines()
        inf.close()
    except:
        print("[ERR] 读取错误")
        exit(3)
print("[INF] 转换完成")
exit(0)
