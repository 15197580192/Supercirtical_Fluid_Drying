import os
import tkinter as tk
from tkinter import filedialog, scrolledtext, messagebox
import subprocess
import threading
import numpy as np
import matplotlib.pyplot as plt

# 设置中文字体支持
plt.rcParams['font.sans-serif'] = ['SimHei']  # 用来正常显示中文标签
plt.rcParams['axes.unicode_minus'] = False  # 用来正常显示负号

parameters = {
    "File_Name": "Ethanol",
    "ra": 1.5000000E-02,
    "Rv": 2.5000000E-02,
    "L": 0.1500000,
    "vz": 0.0024,
    "Cai": 16.50000,
    "Kx": 4.920000000000000E-004,
    "De": 4.70000000000000E-009,
    "Dt": 1.000000E-04,
    "Dr": 5.000000E-04,
    "Dz": 5.000000E-04,
    "总时间步数": 500000000,
    "密度分布输出频率": 200000,
    "溶剂量统计频率": 200000
}  # 保存参数的字典

Profile= "0000000000"

def reset_parameters():
    global parameters  # 声明使用全局变量
    parameters = {
        "File_Name": "Ethanol",
        "ra": 1.5000000E-02,
        "Rv": 2.5000000E-02,
        "L": 0.1500000,
        "vz": 0.0024,
        "Cai": 16.50000,
        "Kx": 4.920000000000000E-004,
        "De": 4.70000000000000E-009,
        "Dt": 1.000000E-04,
        "Dr": 5.000000E-04,
        "Dz": 5.000000E-04,
        "总时间步数": 500000000,
        "密度分布输出频率": 200000,
        "溶剂量统计频率": 200000
    }


# 读取 PARAMETER.dat 文件并解析
def load_parameter_file_parameters():
    try:
        with open("input\PARAMETER.dat", "r", encoding="gbk") as file:
            lines = file.readlines()
        idx = 0
        while idx < len(lines):
            line = lines[idx].strip()
            if line.startswith("File_Name"):
                parameters["File_Name"] = lines[idx + 1].strip()
                idx += 2
            elif line.startswith("凝胶柱半径（ra&Rv, m）"):
                values = list(map(float, lines[idx + 1].strip().split()))  # 拆分为 ra 和 Rv
                parameters["ra"] = values[0]  # 第一个值为 ra
                parameters["Rv"] = values[1]  # 第二个值为 Rv
                idx += 2
            elif line.startswith("凝胶柱高度（L, m）"):
                parameters["L"] = float(lines[idx + 1].strip())
                idx += 2
            elif line.startswith("萃取介质流速(vz, m/s）"):
                parameters["vz"] = float(lines[idx + 1].strip())
                idx += 2
            elif line.startswith("凝胶初始浓度（Cai, kmol/m3）"):
                parameters["Cai"] = float(lines[idx + 1].strip())
                idx += 2
            elif line.startswith("等效传质系数（Kx, m/s）"):
                parameters["Kx"] = float(lines[idx + 1].strip())
                idx += 2
            elif line.startswith("等效扩散系数(De, m2/s)"):
                parameters["De"] = float(lines[idx + 1].strip())
                idx += 2
            elif line.startswith("时间步长(Dt,s)"):
                parameters["Dt"] = float(lines[idx + 1].strip())
                idx += 2
            elif line.startswith("空间步长（Dr&Dz,m）"):
                values = list(map(float, lines[idx + 1].strip().split()))  # 拆分为 Dr&Dz
                parameters["Dr"] = values[0]  # 第一个值为 ra
                parameters["Dz"] = values[1]  # 第二个值为 Rv
                idx += 2
            elif line.startswith("总时间步数，密度分布输出频率，溶剂量统计频率"):
                values = list(map(float, lines[idx + 1].strip().split()))  # 拆分
                parameters["总时间步数"] = int(values[0])
                parameters["密度分布输出频率"] = int(values[1])
                parameters["溶剂量统计频率"] = int(values[2])
                idx += 2
            else:
                idx += 1  # 跳过无关的行

        return parameters, lines

    except FileNotFoundError:
        messagebox.showerror("错误", "PARAMETER.dat 文件未找到！")
        return {}, []


# def load_parameter_file():
#     try:
#         with open("PARAMETER.dat", "r", encoding="gbk") as file:
#             input_text.delete("1.0", tk.END)
#             input_text.insert(tk.END, file.read())
#     except FileNotFoundError:
#         input_text.delete("1.0", tk.END)
#         input_text.insert(tk.END, "PARAMETER.dat not found!")
#
#
def upload_file():
    file_path = filedialog.askopenfilename(filetypes=[("DAT Files", "*.dat"), ("All Files", "*.*")])
    if not file_path:
        return  # 用户取消选择文件

        # 获取当前文件夹中的 "PARAMETER.dat"
    current_dir = os.getcwd()  # 获取当前工作目录
    current_file = os.path.join(current_dir, "input\PARAMETER.dat")

    # 判断是否选择了当前目录下的 PARAMETER.dat
    if os.path.abspath(file_path) == os.path.abspath(current_file):
        messagebox.showerror("错误", "不能上传当前文件夹的 PARAMETER.dat！")
        return
    if file_path:
        with open(file_path, "r", encoding="gbk") as src_file, open("input\PARAMETER.dat", "w",
                                                                    encoding="gbk") as dst_file:
            data = src_file.read()
            dst_file.write(data)
            # input_text.delete("1.0", tk.END)
            # input_text.insert(tk.END, data)
    else:
        messagebox.showinfo("提示", "文件上传失败！")
    load_parameter_file_parameters()


def write_parameters_to_file(filename="input\PARAMETER.dat"):
    # print(parameters)
    with open(filename, 'w') as file:
        file.write(f"File_Name\n{parameters['File_Name']}\n")
        file.write(f"凝胶柱半径（ra&Rv, m）\n{parameters['ra']}  {parameters['Rv']}\n")
        file.write(f"凝胶柱高度（L, m）\n{parameters['L']}\n")
        file.write(f"萃取介质流速(vz, m/s）\n{parameters['vz']}\n")
        file.write(f"凝胶初始浓度（Cai, kmol/m3）\n{parameters['Cai']}\n")
        file.write(f"等效传质系数（Kx, m/s）\n{parameters['Kx']}\n")
        file.write(f"等效扩散系数(De, m2/s)\n{parameters['De']}\n")
        file.write(f"时间步长(Dt,s)\n{parameters['Dt']}\n")
        file.write(f"空间步长（Dr&Dz,m）\n{parameters['Dr']}  {parameters['Dz']}\n")
        file.write(f"总时间步数，密度分布输出频率，溶剂量统计频率\n")
        file.write(f"{parameters['总时间步数']}  {parameters['密度分布输出频率']}  {parameters['溶剂量统计频率']}\n")


# 创建一个事件来控制线程的停止
stop_event = threading.Event()

def stop_test():
    stop_event.set()
    try:
        # 尝试杀死 SFDrun.exe 进程
        subprocess.run(["taskkill", "/F", "/IM", "SFDrun.exe"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except Exception as e:
        print(f"Error killing run.exe: {e}")

def run_test():
    def execute():
        update()
        # 使用 stop_event 来控制线程退出
        write_parameters_to_file()
        process = subprocess.Popen(".\\SFDrun.exe", stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
                                   bufsize=1)

        output_text.delete("1.0", tk.END)
        for line in process.stdout:
            if stop_event.is_set():  # 检查停止事件
                process.kill()  # 强制终止进程
                break
            # 如果line中包含dr,dz,提取出来
            if "dr,dz" in line:
                dr, dz = line.split("   ")[-2:]
                parameters["Dr"] = float(dr)
                parameters["Dz"] = float(dz)
                update_all_entries()
            if "CN" in line:
                # print(line)
                global Profile
                Profile = line.split(":")[-1].strip() # 去掉末尾换行符
            output_text.insert(tk.END, line)
            output_text.see(tk.END)

        process.wait()

    # 创建并启动一个线程
    threading.Thread(target=execute, daemon=True).start()


# # 假设你在某个时刻需要停止这个线程
# def stop_test():
#     stop_event.set()  # 设置停止事件

def on_closing():
    if messagebox.askokcancel("退出", "确认要关闭程序并停止当前计算吗？"):
        stop_test()  # 触发停止机制
        root.destroy()  # 关闭窗口

def plot_concentration():
    """
    根据给定的 Profile 绘制浓度分布图。

    参数:
        Profile (str): Profile编号，例如 "0500000000"
    """
    # 拼接文件名
    filename = "output\Profile" + Profile + ".dat"
    # print(filename)

    try:
        # 读取数据
        data = np.loadtxt(filename, skiprows=1)

        # 提取 R, Z, 浓度
        R = data[:, 0]
        Z = data[:, 1]
        concentration = data[:, 2]

        # 创建网格
        R_unique = np.unique(R)
        Z_unique = np.unique(Z)
        R_grid, Z_grid = np.meshgrid(R_unique, Z_unique)

        # 重塑浓度值为二维数组
        concentration_grid = concentration.reshape(len(Z_unique), len(R_unique))

        # 动态创建颜色级别（9个段）
        min_value = np.min(concentration)
        max_value = np.max(concentration)
        levels = np.linspace(min_value, max_value, 9)

        # 颜色映射
        cmap = plt.get_cmap("jet")

        # 绘制等值线图
        plt.figure(figsize=(8, 8))
        contour = plt.contourf(R_grid, Z_grid, concentration_grid, levels=levels, cmap=cmap)

        # 添加颜色条
        cbar = plt.colorbar(contour)
        cbar.set_label('凝胶柱内浓度')

        # 标注轴和时间
        plt.xlabel("径向位置")
        plt.ylabel("凝胶高度")
        plt.title(f"CN = {Profile}")

        # 显示图像
        plt.show()

    except Exception as e:
        print(f"文件 {filename} 读取失败，错误信息: {e}")

def plot_pressure_density():
    """
    根据给定的 Profile 绘制浓度差和密度差分布图。

    参数:
        Profile (str): Profile编号，例如 "0500000000"
    """
    # 拼接文件名
    filename = "output\press" + Profile + ".dat"

    try:
        # 读取数据
        data = np.loadtxt(filename, skiprows=1)

        # 提取 R, Z, 压力差, 密度差
        R = data[:, 0]
        Z = data[:, 1]
        pressure_diff = data[:, 2]
        density_diff = data[:, 3]

        # 创建网格
        R_unique = np.unique(R)
        Z_unique = np.unique(Z)
        R_grid, Z_grid = np.meshgrid(R_unique, Z_unique)

        # 重塑为二维数组
        pressure_grid = pressure_diff.reshape(len(Z_unique), len(R_unique))
        density_grid = density_diff.reshape(len(Z_unique), len(R_unique))

        # 动态创建颜色级别（9个段）
        pressure_levels = np.linspace(np.min(pressure_diff), np.max(pressure_diff), 9)
        density_levels = np.linspace(np.min(density_diff), np.max(density_diff), 9)

        # 颜色映射
        cmap = plt.get_cmap("jet")

        # 创建绘图窗口
        fig, axes = plt.subplots(1, 2, figsize=(16, 8))

        # 绘制浓度差分布图
        contour1 = axes[0].contourf(R_grid, Z_grid, pressure_grid, levels=pressure_levels, cmap=cmap)
        cbar1 = fig.colorbar(contour1, ax=axes[0])
        cbar1.set_label('凝胶柱内浓度径向梯度')
        axes[0].set_xlabel("径向位置")
        axes[0].set_ylabel("凝胶高度")
        axes[0].set_title(f"Concentration Difference (CN = {Profile})")

        # 绘制密度差分布图
        contour2 = axes[1].contourf(R_grid, Z_grid, density_grid, levels=density_levels, cmap=cmap)
        cbar2 = fig.colorbar(contour2, ax=axes[1])
        cbar2.set_label('凝胶柱内浓度高度梯度')
        axes[1].set_xlabel("径向位置")
        axes[1].set_ylabel("凝胶高度")
        axes[1].set_title(f"Density Difference (CN = {Profile})")

        # 显示图像
        plt.tight_layout()
        plt.show()

    except Exception as e:
        print(f"文件 {filename} 读取失败，错误信息: {e}")


def plot_cv():
    """
    根据给定的 Profile 绘制浓度随 Z 变化的折线图。

    参数:
        Profile (str): Profile编号，例如 "0500000000"
    """
    # 拼接文件名
    filename = "output\Cv" + Profile + ".dat"

    try:
        # 读取数据 (假设每行格式为 Z 浓度)
        data = np.loadtxt(filename, skiprows=1)

        # 提取 Z 和 浓度
        Z = data[:, 0]
        concentration = data[:, 1]

        # 创建折线图
        plt.figure(figsize=(8, 6))
        plt.plot(Z, concentration, marker='o', linestyle='-', color='b', label='Concentration')

        # 添加轴标签和标题
        plt.xlabel("凝胶柱高度")
        plt.ylabel("凝胶柱外表面浓度")
        plt.title(f"CN = {Profile}")
        plt.grid(True)
        plt.legend()

        # 显示图像
        plt.show()

    except Exception as e:
        print(f"文件 {filename} 读取失败，错误信息: {e}")


# 创建主窗口
root = tk.Tk()
root.title("气凝胶干燥")
root.geometry("800x610")

load_parameter_file_parameters()

# 输入
tk.Label(root, text="参数输入", anchor="w", font=("Arial", 14)).pack()


# 创建每一对参数输入框的函数
def create_input_row(label_text_1, param_name_1, label_text_2, param_name_2, entry_list, param_names):
    frame = tk.Frame(root)
    frame.pack(fill=tk.X, padx=10, pady=5)

    # 标签的固定宽度
    label_width = 25  # 适当调整这个值

    # 第一个参数
    label_1 = tk.Label(frame, text=label_text_1, width=label_width, anchor="w")  # 设置标签宽度
    label_1.pack(side=tk.LEFT, padx=5)
    entry_1 = tk.Entry(frame, width=10)  # 设置输入框宽度为10个字符
    entry_1.insert(tk.END, parameters.get(param_name_1, ""))  # 初始化为字典中的值
    entry_1.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)

    # 将输入框添加到 entry_list 和 param_names 中
    entry_list.append(entry_1)
    param_names.append(param_name_1)

    # 第二个参数
    label_2 = tk.Label(frame, text=label_text_2, width=label_width, anchor="w")
    label_2.pack(side=tk.LEFT, padx=5)
    entry_2 = tk.Entry(frame, width=10)  # 设置输入框宽度为30个字符
    entry_2.insert(tk.END, parameters.get(param_name_2, ""))  # 初始化为字典中的值
    entry_2.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)

    # 将输入框添加到 entry_list 和 param_names 中
    entry_list.append(entry_2)
    param_names.append(param_name_2)

    return entry_list, param_names


# 存储所有 entry 和对应的参数名
entry_list = []
param_names = []

# 调用函数示例
entry_list, param_names = create_input_row("凝胶名称:", "File_Name", "凝胶柱半径(ra, m):", "ra", entry_list, param_names)
entry_list, param_names = create_input_row("容器半径(Rv, m):", "Rv", "凝胶柱高度（L, m）:", "L", entry_list, param_names)
entry_list, param_names = create_input_row("萃取介质流速(vz, m/s):", "vz", "凝胶初始浓度（Cai, kmol/m3):", "Cai", entry_list,
                                           param_names)
entry_list, param_names = create_input_row("等效传质系数（Kx, m/s):", "Kx", "等效扩散系数(De, m2/s):", "De", entry_list, param_names)
entry_list, param_names = create_input_row("时间步长(Dt,s):", "Dt", "空间步长（Dr, m）:", "Dr", entry_list, param_names)
entry_list, param_names = create_input_row("空间步长（Dz, m）:", "Dz", "总时间步数:", "总时间步数", entry_list, param_names)
entry_list, param_names = create_input_row("密度分布输出频率:", "密度分布输出频率", "溶剂量统计频率:", "溶剂量统计频率", entry_list, param_names)


# params值更新到entry输入框
def update_all_entries():
    # 遍历 entry_list 更新所有输入框的内容
    for entry in entry_list:
        param_name = param_names[entry_list.index(entry)]  # 获取对应的参数名称
        value = parameters[param_name]  # 获取参数值
        entry.delete(0, tk.END)  # 清除现有的文本
        entry.insert(tk.END, value)  # 插入新的值

# entry输入框值更新到params
def update():
    for entry in entry_list:
        param_name = param_names[entry_list.index(entry)]
        parameters[param_name] = entry.get()

# 重置
def reset():
    reset_parameters()
    update_all_entries()
    write_parameters_to_file()


# input_text = scrolledtext.ScrolledText(root, height=10)
# input_text.pack(fill=tk.BOTH, expand=True)

# 按钮区域
button_frame = tk.Frame(root)
button_frame.pack(pady=5)  # 按钮区域增加上下间距，只调用一次 pack

# 第一行按钮
first_row_frame = tk.Frame(button_frame)  # 创建第一行框架
first_row_frame.pack(side=tk.TOP, pady=5)  # 第一行的按钮放在框架顶部

tk.Button(first_row_frame, text="初始化", command=reset, font=("Arial", 10), width=10, height=1).pack(side=tk.LEFT, padx=40)
tk.Button(first_row_frame, text="执行", command=run_test, font=("Arial", 10), width=10, height=1).pack(side=tk.LEFT, padx=40)
# tk.Button(first_row_frame, text="停止", command=stop_test, font=("Arial", 10), width=10, height=1).pack(side=tk.LEFT, padx=40)  # 新增停止按钮

# 第二行按钮
second_row_frame = tk.Frame(button_frame)  # 创建第二行框架
second_row_frame.pack(side=tk.TOP, pady=5)  # 第二行的按钮放在框架顶部

tk.Button(second_row_frame, text="浓度分布图", command=plot_concentration, font=("Arial", 10), width=10, height=1).pack(side=tk.LEFT, padx=40)
tk.Button(second_row_frame, text="压力分布图（浓度差、密度差）", command=plot_pressure_density, font=("Arial", 10), width=30, height=1).pack(side=tk.LEFT, padx=40)
tk.Button(second_row_frame, text="浓度分布图（外表面）", command=plot_cv, font=("Arial", 10), width=20, height=1).pack(side=tk.LEFT, padx=40)

# 输出框
tk.Label(root, text="结果输出", anchor="w", font=("Arial", 14)).pack()
output_text = scrolledtext.ScrolledText(root, height=10)
output_text.pack(fill=tk.BOTH, expand=True)

# 预加载参数文件
# load_parameter_file()

root.protocol("WM_DELETE_WINDOW", on_closing)

# 运行主循环
root.mainloop()
