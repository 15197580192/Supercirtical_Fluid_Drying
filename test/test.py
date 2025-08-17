import numpy as np
import matplotlib.pyplot as plt

Profile = "0000200000"

def plot_concentration():
    """
    根据给定的 Profile 绘制浓度分布图。

    参数:
        Profile (str): Profile编号，例如 "0500000000"
    """
    # 拼接文件名
    filename = "Profile" + Profile + ".dat"
    # print(filename)

    try:
        # 读取数据
        data = np.loadtxt(filename)

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
        cbar.set_label('Concentration')

        # 标注轴和时间
        plt.xlabel("R")
        plt.ylabel("Z")
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
    filename = "press" + Profile + ".dat"

    try:
        # 读取数据
        data = np.loadtxt(filename)

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
        cbar1.set_label('Concentration Difference')
        axes[0].set_xlabel("R")
        axes[0].set_ylabel("Z")
        axes[0].set_title(f"Concentration Difference (CN = {Profile})")

        # 绘制密度差分布图
        contour2 = axes[1].contourf(R_grid, Z_grid, density_grid, levels=density_levels, cmap=cmap)
        cbar2 = fig.colorbar(contour2, ax=axes[1])
        cbar2.set_label('Density Difference')
        axes[1].set_xlabel("R")
        axes[1].set_ylabel("Z")
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
    filename = "cv" + Profile + ".dat"

    try:
        # 读取数据 (假设每行格式为 Z 浓度)
        data = np.loadtxt(filename)

        # 提取 Z 和 浓度
        Z = data[:, 0]
        concentration = data[:, 1]

        # 创建折线图
        plt.figure(figsize=(8, 6))
        plt.plot(Z, concentration, marker='o', linestyle='-', color='b', label='浓度')

        # 添加轴标签和标题
        plt.xlabel("Z")
        plt.ylabel("Concentration")
        plt.title(f"CN = {Profile}")
        plt.grid(True)
        plt.legend()

        # 显示图像
        plt.show()

    except Exception as e:
        print(f"文件 {filename} 读取失败，错误信息: {e}")


# 示例调用
# plot_pressure_density()
# plot_concentration()
plot_cv()
