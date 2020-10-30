#include <QtGui/QToolButton>
#include <QtGui/QStylePainter>
#include <QtGui/QPainter>
#include <QtGui/QStyleOptionFocusRect>
#include <QtGui/QMouseEvent>
#include <cmath>
#include "aplotter.h"

APlotter::APlotter(QWidget *parent) :
    QWidget(parent)
{
    setBackgroundRole(QPalette::Light);
    setAutoFillBackground(true);
    setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    setFocusPolicy(Qt::StrongFocus);
    rubberBandIsShown = false;
    curveMode = false;

    zoomInButton = new QToolButton(this);
    zoomInButton->setIcon(QIcon(":/images/zoom-in.png"));
    zoomInButton->adjustSize();
    connect(zoomInButton, SIGNAL(clicked()), this, SLOT(zoomIn()));

    zoomOutButton = new QToolButton(this);
    zoomOutButton->setIcon(QIcon(":/images/zoom-out.png"));
    zoomOutButton->adjustSize();
    connect(zoomOutButton, SIGNAL(clicked()), this, SLOT(zoomOut()));

    blackPen.setColor(Qt::black);
    blackPen.setStyle(Qt::SolidLine);
    blackPen.setWidth(1);

    bluePen.setColor(Qt::blue);
    bluePen.setStyle(Qt::SolidLine);
    bluePen.setWidth(2);

    setPlotSettings(APlotSettings());
}

void APlotter::setPlotSettings(const APlotSettings &settings)
{
    zoomStack.clear();
    zoomStack.append(settings);
    curZoom = 0;
    zoomInButton->hide();
    zoomOutButton->hide();
    refreshPixmap();
}

void APlotter::zoomOut()
{
    if (curZoom > 0)
    {
        --curZoom;
        zoomOutButton->setEnabled(curZoom > 0);
        zoomInButton->setEnabled(true);
        zoomInButton->show();
        refreshPixmap();
    }
}

void APlotter::zoomIn()
{
    if (curZoom < zoomStack.count() - 1)
    {
        ++curZoom;
        zoomInButton->setEnabled(curZoom < zoomStack.count() - 1);
        zoomOutButton->setEnabled(true);
        zoomOutButton->show();
        refreshPixmap();
    }
}

void APlotter::setCurveData(int id, const QVector<QPointF> &data)
{
    curveMap[id] = data;
    refreshPixmap();
}

void APlotter::setCurveLabel(int id, const QString &label)
{
    labelMap[id] = label;
    refreshPixmap();
}

void APlotter::clearCurve(int id)
{
    curveMap.remove(id);
    labelMap.remove(id);
    refreshPixmap();
}

QSize APlotter::minimumSizeHint() const
{
    return QSize(6 * Margin, 4 * Margin);
}

QSize APlotter::sizeHint()
{
    return QSize(12 * Margin, 8 * Margin);
}

void APlotter::paintEvent(QPaintEvent *)
{
    QStylePainter painter(this);
    painter.drawPixmap(0, 0, pixmap);

    if (rubberBandIsShown)
    {
        painter.setPen(blackPen);
        painter.drawRect(rubberBandRect.normalized().adjusted(0, 0, -1, -1));
    }

//    if (hasFocus())
//    {
//        QStyleOptionFocusRect option;
//        option.initFrom(this);
//        option.backgroundColor = palette().dark().color();
//        painter.drawPrimitive(QStyle::PE_FrameFocusRect, option);
//    }
}

void APlotter::resizeEvent(QResizeEvent *)
{
    canvasRect.setRect(Margin, Margin * 0.25, width() - Margin * 1.25, height() - Margin);
    dataRect = canvasRect.adjusted(DataMargin, DataMargin, -DataMargin, -DataMargin);

    int x = canvasRect.right() - (zoomInButton->width() + zoomOutButton->width() + 10);
    int y = canvasRect.top() + 10;
    zoomInButton->move(x, y);
    zoomOutButton->move(x + zoomInButton->width() + 5, y);

    refreshPixmap();
}

void APlotter::mousePressEvent(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton)
    {
        if (canvasRect.contains(event->pos()))
        {
            rubberBandIsShown = true;
            rubberBandRect.setTopLeft(event->pos());
            rubberBandRect.setBottomRight(event->pos());
            updateRubberBandRegion();
            setCursor(Qt::CrossCursor);
        }
    }
}

void APlotter::mouseMoveEvent(QMouseEvent *event)
{
    if (rubberBandIsShown)
    {
        updateRubberBandRegion();
        rubberBandRect.setBottomRight(event->pos());
        updateRubberBandRegion();
    }
}

void APlotter::mouseReleaseEvent(QMouseEvent *event)
{
    if ((event->button() == Qt::LeftButton) && rubberBandIsShown)
    {
        rubberBandIsShown = false;
        updateRubberBandRegion();
        unsetCursor();

        QRect rect = rubberBandRect.normalized();
        if (rect.width() < 4 || rect.height() < 4)
            return;
        rect.translate(-dataRect.left(), -dataRect.top());

        APlotSettings prevSettings = zoomStack[curZoom];
        APlotSettings settings;
        double dx = prevSettings.spanX() / dataRect.width();
        double dy = prevSettings.spanY() / dataRect.height();
        settings.minX = prevSettings.minX + dx * rect.left();
        settings.maxX = prevSettings.minX + dx * rect.right();
        settings.minY = prevSettings.maxY - dy * rect.bottom();
        settings.maxY = prevSettings.maxY - dy * rect.top();
        settings.adjust();

        zoomStack.resize(curZoom + 1);
        zoomStack.append(settings);
        zoomIn();
    }
}

void APlotter::keyPressEvent(QKeyEvent *event)
{
    switch(event->key())
    {
    case Qt::Key_Plus:
        zoomIn();
        break;
    case Qt::Key_Minus:
        zoomOut();
        break;
    case Qt::Key_Left:
        zoomStack[curZoom].scroll(-1, 0);
        refreshPixmap();
        break;
    case Qt::Key_Right:
        zoomStack[curZoom].scroll(+1, 0);
        refreshPixmap();
        break;
    case Qt::Key_Down:
        zoomStack[curZoom].scroll(0, -1);
        refreshPixmap();
        break;
    case Qt::Key_Up:
        zoomStack[curZoom].scroll(0, +1);
        refreshPixmap();
        break;
    default:
        QWidget::keyPressEvent(event);
    }
}

void APlotter::wheelEvent(QWheelEvent *event)
{
    int numDegrees = event->delta() / 8;
    int numTicks = numDegrees / 15;

    if (event->orientation() == Qt::Horizontal)
    {
        zoomStack[curZoom].scroll(numTicks, 0);
    }
    else
    {
        zoomStack[curZoom].scroll(0, numTicks);
    }
    refreshPixmap();
}

void APlotter::updateRubberBandRegion()
{
    QRect rect = rubberBandRect.normalized();
    update(rect.left(), rect.top(), rect.width(), 1);
    update(rect.left(), rect.top(), 1, rect.height());
    update(rect.left(), rect.bottom(), rect.width(), 1);
    update(rect.right(), rect.top(), 1, rect.height());
}

void APlotter::refreshPixmap()
{
    pixmap = QPixmap(size());
    pixmap.fill(this, 0, 0);
    QPainter painter(&pixmap);
    painter.initFrom(this);

    QFont font = painter.font();
    font.setPointSize(font.pointSize() - 2);
    painter.setFont(font);

    drawGrid(&painter);

    if (curveMode)
        drawCurves(&painter);
    else
        drawPoints(&painter);

    drawLabels(&painter);

    update();
}

void APlotter::drawGrid(QPainter *painter)
{
    QRect &rect = dataRect;

    if (!rect.isValid())
        return;

    APlotSettings settings = zoomStack[curZoom];

    double dStepX = settings.spanX() / settings.numXTicks;
    double dStepY = settings.spanY() / settings.numYTicks;

    double dFinalStepX = pow(10.0, floor(log10(dStepX)));
    double dFinalStepY = pow(10.0, floor(log10(dStepY)));

    if (5 * dFinalStepX < dStepX)
        dFinalStepX *= 5;
    else if (2 * dFinalStepX < dStepX)
        dFinalStepX *= 2;

    if (5 * dFinalStepY < dStepY)
        dFinalStepY *= 5;
    else if (2 * dFinalStepY < dStepY)
        dFinalStepY *= 2;

    dStepX = dFinalStepX;
    dStepY = dFinalStepY;

    double dMinX = ceil(settings.minX / dStepX) * dStepX;
    double dMinY = ceil(settings.minY / dStepY) * dStepY;

    double dTickX = 0.0;
    double dTickY = 0.0;

    settings.numXTicks = settings.spanX() / dStepX;
    settings.numYTicks = settings.spanY() / dStepY;

    for (int i = 0; i <= settings.numXTicks; ++i)
    {
        // get tick value
        dTickX = dMinX + i * dStepX;
        double label = dTickX;

        // get relative position
        dTickX = dTickX - settings.minX;
        int x = rect.left() + (dTickX * (rect.width() - 1) / settings.spanX());

        if (x > canvasRect.right())
            break;

        painter->setPen(blackPen);
        painter->drawLine(x, canvasRect.bottom(), x, canvasRect.bottom() + 5);
        painter->drawText(x - 50, canvasRect.bottom() + 5, 100, 15,
                          Qt::AlignHCenter | Qt::AlignTop, QString("%1").arg(label));
    }

    for (int j = 0; j <= settings.numYTicks; ++j)
    {
        // get tick value
        dTickY = dMinY + j * dStepY;
        double label = dTickY;

        // get relative position
        dTickY = dTickY - settings.minY;
        int y = rect.bottom() - (dTickY * (rect.height() - 1) / settings.spanY());

        if (y < canvasRect.top())
            break;

        painter->setPen(blackPen);
        painter->drawLine(canvasRect.left() - 5, y, canvasRect.left(), y);
        painter->drawText(canvasRect.left() - Margin, y - 10, Margin - 5, 20,
                          Qt::AlignRight | Qt::AlignVCenter, QString("%1").arg(label));
    }

    painter->drawRect(canvasRect.adjusted(0, 0, -1, -1));
}

void APlotter::drawCurves(QPainter *painter)
{
    static const QColor colorForIds[6] = {
        Qt::red, Qt::green, Qt::blue, Qt::cyan, Qt::magenta, Qt::yellow };

    QRect &rect = dataRect;
    if (!rect.isValid())
        return;

    APlotSettings settings = zoomStack[curZoom];
    painter->setClipRect(canvasRect.adjusted(1, 1, -1, -1));

    QMapIterator<int, QVector<QPointF> > i(curveMap);
    while (i.hasNext())
    {
        i.next();

        int id = i.key();
        const QVector<QPointF> &data = i.value();
        QPolygonF polyline(data.count());

        for (int j = 0; j < data.count(); ++j)
        {
            double dx = data[j].x() - settings.minX;
            double dy = data[j].y() - settings.minY;
            double x = rect.left() + (dx * (rect.width() - 1) / settings.spanX());
            double y = rect.bottom() - (dy * (rect.height() - 1) / settings.spanY());
            polyline[j] = QPointF(x, y);
        }

        painter->setPen(colorForIds[uint(id) % 6]);
        painter->drawPolyline(polyline);
    }
}

void APlotter::drawPoints(QPainter *painter)
{
    static const QColor colorForIds[6] = {
        Qt::red, Qt::green, Qt::blue, Qt::cyan, Qt::magenta, Qt::yellow };

    QRect &rect = dataRect;
    if (!rect.isValid())
        return;

    APlotSettings settings = zoomStack[curZoom];
    painter->setClipRect(canvasRect.adjusted(1, 1, -1, -1));

    QMapIterator<int, QVector<QPointF> > i(curveMap);
    while (i.hasNext())
    {
        i.next();

        int id = i.key();
        const QVector<QPointF> &data = i.value();
        QPolygonF polyline(data.count());

        for (int j = 0; j < data.count(); ++j)
        {
            double dx = data[j].x() - settings.minX;
            double dy = data[j].y() - settings.minY;
            double x = rect.left() + (dx * (rect.width() - 1) / settings.spanX());
            double y = rect.bottom() - (dy * (rect.height() - 1) / settings.spanY());
            polyline[j] = QPointF(x, y);
        }

        painter->setPen(colorForIds[uint(id) % 6]);
        QPen pen = painter->pen();
        pen.setWidth(PointSize);
        painter->setPen(pen);
        painter->drawPoints(polyline);
    }
}

void APlotter::drawLabels(QPainter *painter)
{
    static const QColor colorForIds[6] = {
        Qt::red, Qt::green, Qt::blue, Qt::cyan, Qt::magenta, Qt::yellow };

    QPen pointPen;
    pointPen.setWidth(PointSize * 1.5);

    QMapIterator<int, QString> labelIter(labelMap);
    int n = 0;      // label counter
    int h = 20;     // height of label rectangle
    int w = 100;    // width of label rectangle
    int mx = 10;    // x margin
    int my = 20;    // y margin
    while (labelIter.hasNext())
    {
        labelIter.next();
        int id = labelIter.key();
        n++;

        painter->setPen(blackPen);
        painter->drawText(canvasRect.right() - mx * 2 - w, canvasRect.top() + my + n * h, w, h,
                          Qt::AlignRight | Qt::AlignVCenter, labelIter.value());

        pointPen.setColor(colorForIds[id % 6]);
        painter->setPen(pointPen);
        painter->drawPoint(canvasRect.right() - mx, canvasRect.top() + my + n * h + h / 2);
    }
}

void APlotter::setCurveMode(bool enabled)
{
    curveMode = enabled;
    refreshPixmap();
}

APlotSettings::APlotSettings()
{
    minX = 0.0;
    maxX = 10.0;
    numXTicks = 5;

    minY = 0.0;
    maxY = 10.0;
    numYTicks = 5;
}

void APlotSettings::scroll(int dx, int dy)
{
    double stepX = spanX() / numXTicks;
    minX += dx * stepX;
    maxX += dx * stepX;

    double stepY = spanY() / numYTicks;
    minY += dy * stepY;
    maxY += dy * stepY;
}

void APlotSettings::adjust()
{
    adjustAxis(minX, maxX, numXTicks);
    adjustAxis(minY, maxY, numYTicks);
}

void APlotSettings::adjustAxis(double &min, double &max, int &numTicks)
{
    const int MinTicks = 3;
    double grossStep = (max - min) / MinTicks;
    double step = pow(10.0, floor(log10(grossStep)));

    if (5 * step < grossStep)
    {
        step *= 5;
    }
    else if (2 * step < grossStep)
    {
        step *= 2;
    }

    numTicks = int (ceil(max / step) - floor(min / step));
    if (numTicks < MinTicks)
        numTicks = MinTicks;

    min = floor(min / step) * step;
    max = ceil(max / step) * step;
}
