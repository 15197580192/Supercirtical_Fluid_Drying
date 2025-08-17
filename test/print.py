import numpy as np
import matplotlib.pyplot as plt


def plot_concentration(Profile):
    """
    根据给定的 Profile 绘制浓度分布图。

    参数:
        Profile (str): Profile编号，例如 "0500000000"
    """
    # 拼接文件名
    filename = "Profile" + Profile + ".dat"

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
        cbar.set_label('浓度')

        # 标注轴和时间
        plt.xlabel("R")
        plt.ylabel("Z")
        plt.title(f"CN = {Profile}")

        # 显示图像
        plt.show()

    except Exception as e:
        print(f"文件 {filename} 读取失败，错误信息: {e}")


# 示例调用
plot_concentration("0500000000")
