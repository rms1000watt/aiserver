#ifndef APLOTTER_H
#define APLOTTER_H

#include <QtCore/QMap>
#include <QtCore/QVector>
#include <QtGui/QFont>
#include <QtGui/QPen>
#include <QtGui/QPixmap>
#include <QtGui/QWidget>

class QToolButton;
class APlotSettings;

class APlotter : public QWidget
{
    Q_OBJECT

public:
    APlotter(QWidget *parent = 0);

    void setPlotSettings(const APlotSettings &settings);
    void setCurveData(int id, const QVector<QPointF> &data);
    void setCurveLabel(int id, const QString &label);
    void setCurveMode(bool enabled);

    void clearCurve(int id);    

    virtual QSize minimumSizeHint() const;
    virtual QSize sizeHint();

signals:

public slots:
    void zoomIn();
    void zoomOut();

protected:
    void paintEvent(QPaintEvent *);
    void resizeEvent(QResizeEvent *);
    void mousePressEvent(QMouseEvent *);
    void mouseMoveEvent(QMouseEvent *);
    void mouseReleaseEvent(QMouseEvent *);
    void keyPressEvent(QKeyEvent *);
    void wheelEvent(QWheelEvent *);

private:
    void updateRubberBandRegion();
    void refreshPixmap();
    void drawGrid(QPainter *painter);
    void drawCurves(QPainter *painter);
    void drawPoints(QPainter *painter);
    void drawLabels(QPainter *painter);

    enum { DataMargin = 10 };
    enum { Margin = 50 };
    enum { PointSize = 4 };

    QToolButton *zoomInButton;
    QToolButton *zoomOutButton;
    QMap<int, QVector<QPointF> > curveMap;
    QMap<int, QString> labelMap;
    QVector<APlotSettings> zoomStack;
    int curZoom;
    bool rubberBandIsShown;
    bool curveMode;
    QRect rubberBandRect;
    QRect canvasRect;
    QRect dataRect;
    QPixmap pixmap;
    QFont textFont;
    QPen blackPen;
    QPen bluePen;
};

class APlotSettings
{
public:
    APlotSettings();

    void scroll(int dx, int dy);
    void adjust();

    double spanX() const { return maxX - minX; }
    double spanY() const { return maxY - minY; }

    double minX;
    double maxX;
    int numXTicks;
    double minY;
    double maxY;
    int numYTicks;

private:
    static void adjustAxis(double &min, double &max, int &numTicks);
};

#endif // APLOTTER_H
