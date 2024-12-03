import time
import random
import psutil
import gc
from sort import three_way_merge_sort

# Test ve zaman ölçümü
def test_sorting(size):
    arr = [random.randint(1, 1000) for _ in range(size)]  # Rastgele veri oluştur

    # Bellek kullanımını başlat
    process = psutil.Process()
    gc.collect()
    memory_before = process.memory_info().rss 
    start_time = time.perf_counter()  # Başlangıç zamanını al
    sorted_arr = three_way_merge_sort(arr)  # Algoritmayı çalıştır
    end_time = time.perf_counter()  # Bitiş zamanını al

    # Bellek kullanımını bitir
    memory_after = process.memory_info().rss  # Bellek kullanımı
    
    # Bellek farkını hesapla
    memory_used = memory_after - memory_before
    duration = (end_time - start_time)*1_000_000  # Geçen süreyi döndür

    print(f"-- Veri Seti Boyutu: {size} - Süre: {duration:.6f} µs - Kullanılan Bellek: {memory_used:.6f} KB")

    return duration, memory_used


def main():
    # Veri seti boyutları
    sizes = [1000, 10000, 100000, 1000000]
    for size in sizes:
        durations = []
        memories = []
        for i in range(5):
            duration, memory = test_sorting(size)
            durations.append(duration)
            memories.append(memory)
        avg_duration = sum(durations)/len(durations)
        avg_memory = sum(memories)/len(memories)

        print(f"== Veri Seti Boyutu: {size} - Ortalama Süre: {avg_duration:.6f}µs - Ortalama Kullanılan Bellek: {avg_memory:.6f}KB")
        

if __name__ == "__main__":
    main()
