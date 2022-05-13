############ By I.K.H.O.T ############

from sys import argv

if not len(argv)==3:
    print("[ERR] 过多或过少参数")
    exit(1)
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
maps={}
mapz={}
num=-1
fnd=False
tmp=""
try:
    for i in inc:
        if i.find("BuildingSurface:Detailed,")==0:
            fnd=True
        else:
            if fnd:
                if not i.find("  ")==0:
                    fnd=False
                    if num<0:
                        print("[ERR] 输入不完整")
                        exit(3)
                    else:
                        maps[num]=tmp
                    tmp=""
                    num=-1
                else:
                    if i.split(",")[0].find("  Surface ")>=0 and i.find("Name")>=0:
                        num=int(i[i.find("Surface")+7:i[:i.rfind("!-")].rfind(",")])
                    tmp+=i
    for i in inc:
        if i.find("Zone,")==0:
            fnd=True
        else:
            if fnd:
                if not i.find("  ")==0:
                    fnd=False
                    if num<0:
                        print("[ERR] 输入不完整")
                        exit(3)
                    else:
                        mapz[num]=tmp
                    tmp=""
                    num=-1
                else:
                    if i.split(",")[0].find("  Thermal Zone ")>=0:
                        num=int(i[i.find("Thermal Zone")+12:i[:i.rfind("!-")].rfind(",")])
                    tmp+=i
except:
    print("[ERR] 转换错误")
    exit(3)
kys=list(maps.keys())
kys.sort()
kyz=list(mapz.keys())
kyz.sort()
try:
    ouf=open(argv[2],"w")
except:
    print("[ERR] 无法访问文件")
    exit(1)
try:
    for i in kys:
        ouf.writelines("BuildingSurface:Detailed,\n")
        ouf.writelines(maps[i])
        ouf.writelines("\n")
    for i in kyz:
        ouf.writelines("Zone,\n")
        ouf.writelines(mapz[i])
        ouf.writelines("\n")
except:
    print("[ERR] 写入错误")
    exit(2)
try:
    ouf.close()
except:
    print("[ERR] 操作错误")
    exit(3)
print("[INF] 转换完成")
exit(0)
