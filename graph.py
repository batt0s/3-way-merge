import matplotlib.pyplot as plt

# Veri
data_sizes = ['10k', '100k', '1000k']
python_mem = [50790.4, 174489.6, 8032256.0]
haskell_mem = [0, 0, 0]
go_mem = [737088, 9060784, 109763520]
go_conc_mem = [738624, 9070256, 109822816]

x = range(len(data_sizes))

# Gruplandırılmış Çubuk Grafiği
plt.bar([p - 0.2 for p in x], python_mem, width=0.2, label="Python", align='center')
plt.bar(x, haskell_mem, width=0.2, label="Haskell", align='center')
plt.bar([p + 0.2 for p in x], go_mem, width=0.2, label="Go", align='center')
plt.bar([p + 0.4 for p in x], go_conc_mem, width=0.2, label="Go (Concurrent)", align='center')


plt.xlabel("Data Size")
plt.ylabel("Memory (bytes)")
plt.title("Performance Comparison: Memory")
plt.xticks(x, data_sizes)
plt.legend()
plt.show()
