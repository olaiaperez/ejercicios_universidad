#include <xc.h>
#define _XTAL_FREQ 1000000 
#pragma config LVP=OFF, WDTEN=OFF, FOSC=INTIO67, PBADEN=OFF

char mensaje_T[25] = {'*', 'A', '0', '0', '0', '0', '*', ',','*', 'B', '0', '0', '0', '1', '*', ',','*', 'C', '0', '0', '0', '2', '*',0x04};
char mensaje_R[7];
char *punt_T = &mensaje_T[1];
char *punt_R = &mensaje_R[0];
char primero = 0;
int unsigned contador = 0;
int unsigned aux1 = 0;

//Variables de contadores
unsigned int tiempo0 = 53034; //50ms a 64 PRESCALER
unsigned int tiempo1 = 63536; //1ms  a 8 PRESCALER
unsigned int tiempo3 = 55536; //5ms  a 8 PRESCALER

unsigned int resultado = 0;
char marcha = 0;
unsigned int i = 0;
unsigned int estado = 1;
unsigned int cont_espera = 0, cont_latidos = 0;
unsigned int p_act = 0,p_ant= 0;
unsigned int p_sist = 1;
unsigned int p_diast = 2;
unsigned int p_min = 260;
unsigned int p_max = 700;
unsigned int sist = 0;
unsigned int pul1 = 0;
unsigned int cuenta = 0;
const unsigned fin_on=8,fin_off=1000; //CONTROL EV

void ini_PORTS(void) {
    unsigned valor;
    TRISD = 0xf0;
    LATD = 0xf0;
    TRISB = 0xf0;
    
    TRISDbits.TRISD4 = 0;
    TRISDbits.TRISD5 = 0;
    
    LATDbits.LATD4 = 0;
    LATDbits.LATD5 = 0;
    
    ANSELHbits.ANS11 = 0;
    LATB = 0xf0;
    LATBbits.LATB0 = 1;
    IOCBbits.IOCB4 = 1;
    valor = PORTB;
    
    LATDbits.LATD0 = 0;
    TRISBbits.TRISB0 = 1;
    ANSELHbits.ANS12 = 0;
    
    INTCONbits.INT0IF = 0;
    INTCONbits.INT0IE = 1;
    
    INTCON2bits.RBIP = 1;
    INTCONbits.RBIF = 0;
    INTCONbits.RBIE = 1;
}
void ini_CVREF(void){
    CVRCON = 0;
    CVRCONbits.CVRSS = 0;
    CVRCONbits.CVRR = 1;
    CVRCONbits.CVR = 13; // 1.74V
    CVRCONbits.CVROE = 1;
    CVRCONbits.CVREN = 1;
}
void ini_TMR0(void) {
    T0CON = 0;
    T0CONbits.T0PS = 0b101;
    INTCONbits.TMR0IF = 0;
    INTCONbits.TMR0IE = 1;
    TMR0H = tiempo0 >> 8;
    TMR0L = tiempo0;

    T0CONbits.TMR0ON = 1;
}

void ini_TMR1(void) {
    T1CON = 0;
    T1CONbits.RD16 = 1;
    T1CONbits.T1CKPS = 0b11;

    PIR1bits.TMR1IF = 0;
    PIE1bits.TMR1IE = 1;
    IPR1bits.TMR1IP = 1;

    TMR1H = tiempo1 >> 8;
    TMR1L = tiempo1;


    T1CONbits.TMR1ON = 0;
}
void ini_TMR3(void) {
    T3CON = 0;
    T3CONbits.RD16 = 1;
    T3CONbits.T3CKPS = 3;
    IPR2bits.TMR3IP = 1;
    PIR2bits.TMR3IF = 0;
    PIE2bits.TMR3IE = 1;
    
}

void ini_CAD(void) {
    ADCON0 = 0;
    ADCON0bits.ADON = 1;
    ADCON1 = 0;
    ADCON2 = 0;
    ADCON2bits.ADFM = 1;
    ADCON1bits.VCFG0 =0;
    ADCON1bits.VCFG1 =1;
    ADCON2bits.ADCS = 0b110;
    //ADCON2bits.ACQT = 0b111;
    ADCON0bits.CHS = 1;
    

    TRISAbits.TRISA0 = 1;
    ANSELbits.ANS0 = 1;
    
    TRISAbits.TRISA1 = 1;
    ANSELbits.ANS1 = 1;
    
//    TRISAbits.TRISA2 = 1;
//    ANSELbits.ANS2 = 1;
    
    PIE1bits.ADIE = 1;
    PIR1bits.ADIF = 0;
    IPR1bits.ADIP = 1;

}

void ini_usart(void) {
    BAUDCONbits.BRG16 = 1; //BaudRate Generator modo 16bits
    TXSTAbits.BRGH = 1; //Modo alta velocidad
    SPBRG = 0x81; //Establecer velocidad 96000Bd 1665
    SPBRGH = 0x06;

    TRISCbits.TRISC6 = 1; //TX 
    TRISCbits.TRISC7 = 1; //RX

    TXSTAbits.SYNC = 0; //Modo asíncrono
    RCSTAbits.SPEN = 1; //Habilitar funcionamiento

    TXSTAbits.TXEN = 1; //Permitir transmisión
    RCSTAbits.CREN = 1; //Permitir recepción

    IPR1bits.RC1IP = 1; //Prioridad alta recepción
    IPR1bits.TX1IP = 1; //Prioridad alta transmisión
    PIR1bits.RC1IF = 0; //Borrado flag recepción
    PIE1bits.RC1IE = 1; //Permitir interrupciones recepción
}

void f_marcha_paro(void) {
    if (marcha == 0) {
        marcha = 1;
        estado = 1;
        LATD = 0;
        LATDbits.LATD0 = 1;
        T1CONbits.TMR1ON = 0;
    } else {
        marcha = 0;
        estado = 1;
        LATD = 0;
        T1CONbits.TMR1ON = 0;
    }

}
void __interrupt(high_priority) interrupciones(void) {
    if (INTCONbits.RBIF == 1 && INTCONbits.RBIE == 1) {
        TMR3H = tiempo3 >> 8;
        TMR3L = tiempo3;
        PIR2bits.TMR3IF = 0;
        PIE2bits.TMR3IE = 1;
        T3CONbits.TMR3ON = 1;
        INTCONbits.RBIF = 0;
        INTCONbits.RBIE = 0;
    }
    if (INTCONbits.INT0IF == 1 && INTCONbits.INT0IE == 1) {
        TMR3H = tiempo3 >> 8;
        TMR3L = tiempo3;
        PIR2bits.TMR3IF = 0;
        PIE2bits.TMR3IE = 1;
        T3CONbits.TMR3ON = 1;
        pul1 = 1;
        INTCONbits.INT0IF = 0;
        INTCONbits.INT0IE = 0;
    }
    
    if (PIR2bits.TMR3IF == 1 && PIE2bits.TMR3IE == 1) {
        PIR2bits.TMR3IF = 0;
        PIE2bits.TMR3IE = 0;
        T3CONbits.TMR3ON = 0;
        if (pul1 == 1) {
            f_marcha_paro();
            pul1 = 0;
        }
        if (PORTBbits.RB4 == 0) {

            if (sist == 0) {
                p_sist = p_act;
                sist = 1;
            } else {
                p_diast = p_act;

            }
        }

        INTCONbits.INT0IF = 0;
        INTCONbits.INT0IE = 1;
        INTCONbits.RBIF = 0;
        INTCONbits.RBIE = 1;
    }
    
    if ((INTCONbits.TMR0IF == 1)&&(INTCONbits.TMR0IE == 1)) {
        INTCONbits.TMR0IF = 0;
        TMR0H = tiempo0 >> 8;
        TMR0L = tiempo0;
        ADCON0bits.GO = 1;
        if(marcha == 1){
            switch (estado) {
                    //Estado 1 inflar
                case 1:
                    if (p_act < p_max) {
                        //INFLAR
                        LATDbits.LATD4 = 1;
                        LATDbits.LATD1 = 1;

                    } else {
                        //Parar bomba y iniciar espera
                        LATDbits.LATD4 = 0;
                        LATDbits.LATD1 = 0;
                        LATDbits.LATD2 = 1;
                        estado++;


                    }
                    break;
                    //Esperar
                case 2:
                    if (cont_espera <= 50) {
                        cont_espera++;
                    } else {
                        //Empezar desinflado controlado
                        estado++;
                        LATDbits.LATD2 = 0;
                        LATDbits.LATD3 = 1;

                        T1CONbits.TMR1ON = 1;

                    }
                    break;
                    //Desinflado y mediciónes
                case 3:
                    //Detección Latidos
                    cont_latidos++;
                    if (p_ant <= p_act) {
                        p_ant = p_act;
                        if (cont_latidos >= 20) {
                            p_diast = p_act;
                            estado++;
                            T1CONbits.TMR1ON = 0;
                            LATDbits.LATD3 = 0;
                        }
                    } else {
                        //Primer Latido -> P sistólica
                        if (sist == 0) {
                            p_sist = p_act;
                            sist = 1;
                        }
                        p_ant = p_act;
                        cont_latidos = 0;
                    }
                    break;
                    //Desinflado completo
                case 4:
                    if (p_act > p_min) {
                        LATDbits.LATD5 = 1;
                    } else {
                        LATDbits.LATD5 = 0;
                        estado++;
                    }
                    break;
                    //Estado 5 Enviar tablet 
                case 5:
                    Nop();

                    break;
            }
        }
    }
    if ((PIR1bits.TMR1IF == 1)&&(PIE1bits.TMR1IE == 1)) {
        //Control de la electroVALV
        TMR1H = tiempo1 >> 8;
        TMR1L = tiempo1;
        PIR1bits.TMR1IF = 0;
        cuenta++;
        if (cuenta < fin_on) LATDbits.LATD5 = 1;
        else {
            LATDbits.LATD5 = 0;
            if (cuenta == fin_off) {
                cuenta = 0;
                LATDbits.LATD5 = 1;
            }
        }

    }

    
    
    if (PIR1bits.RC1IF == 1 && PIE1bits.RC1IE == 1) {
        if (RCREG == 'D') {
            f_marcha_paro();
        }
    }
    if (PIR1bits.ADIF == 1 && PIE1bits.ADIE == 1) {
        PIR1bits.ADIF = 0;
        p_act = (256 * ADRESH) + ADRESL;
        if (TX1IE == 0) {
            resultado = (256 * ADRESH) + ADRESL;
            aux1 = resultado;
            for (i = 0; i < 4; i++) {
                mensaje_T[5 - i] = aux1 % 10 + 0x30;
                aux1 = aux1 / 10;
            }
            aux1 = p_diast;
            for (i = 0; i < 4; i++) {
                mensaje_T[13 - i] = aux1 % 10 + 0x30;
                aux1 = aux1 / 10;
            }
            aux1 = p_sist;
            for (i = 0; i < 4; i++) {
                mensaje_T[21 - i] = aux1 % 10 + 0x30;
                aux1 = aux1 / 10;
            }
            TXREG = mensaje_T[0];
            punt_T = &mensaje_T[1];
            PIE1bits.TX1IE = 1;
        }
    }

    if (PIR1bits.TX1IF == 1 && PIE1bits.TX1IE == 1) {
        TXREG = *punt_T;
        if (*punt_T == 0x04) {
            PIE1bits.TX1IE = 0;
        } else {
            punt_T++;
        }
    }


}

void main(void) {
    OSCCONbits.IRCF = 7; //Seleccion oscilador interno 16Mhz
    OSCTUNEbits.PLLEN = 1; //No permitir funcionamiento PLL
    OSCCONbits.SCS = 0; //Seleccion de fuente de oscilador interno

    RCONbits.IPEN = 0;
    INTCONbits.GIEH = 1;
    INTCONbits.GIEL = 1;

    ini_PORTS();
    ini_TMR0();
    ini_TMR1();
    ini_TMR3();
    ini_CAD();
    ini_usart();
    ini_CVREF();

    while (1);
}
